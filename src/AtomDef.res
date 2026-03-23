// type level stuff to enable well-typed coercions
type typeTag<_> = ..
type rec eq<_, _> = Refl: eq<'a, 'a>

module type ATOM = {
  type t
  type subst = Map.t<int, t>
  let tagEq: typeTag<'a> => option<eq<t, 'a>>
  let tag: typeTag<t>
  let unify: (t, t, ~gen: ref<int>=?) => Seq.t<subst>
  let prettyPrint: (t, ~scope: array<string>) => string
  let parse: (string, ~scope: array<string>, ~gen: ref<int>=?) => result<(t, string), string>
  let substitute: (t, subst) => t
  let upshift: (t, int, ~from: int=?) => t
  // used for when trying to substitute a variable of the wrong type
  let lowerVar: int => option<t>
  let lowerSchematic: (int, array<int>) => option<t>
  let substDeBruijn: (t, array<option<t>>, ~from: int=?) => t
  let concrete: t => bool
}

// coercion<t> represents a coercion from some type 'a to t
type rec coercion<_> = Coercion(typeTag<'a>, 'a => option<'b>): coercion<'b>
module type COERCIBLE_ATOM = {
  include ATOM
  let coercions: array<coercion<t>>
}

module MakeCoercible = (
  Atom: ATOM,
  Coercions: {
    let coercions: array<coercion<Atom.t>>
  },
): (COERCIBLE_ATOM with type t = Atom.t) => {
  include Atom
  let coercions = Coercions.coercions
}

exception MatchCombineAtomVar
exception MatchCombineAtomSchematic

module CombineAtom = (Left: COERCIBLE_ATOM, Right: COERCIBLE_ATOM): {
  type base =
    | Left(Left.t)
    | Right(Right.t)
    // neither of the below should appear organically. they're purely so we can lower
    // substitutions to both Left.t and Right.t
    | Var({idx: int})
    | Schematic({schematic: int, allowed: array<int>})
  include COERCIBLE_ATOM with type t = base
  let match: (t, Left.t => 'a, Right.t => 'a) => 'a
} => {
  type base =
    | Left(Left.t)
    | Right(Right.t)
    | Var({idx: int})
    | Schematic({schematic: int, allowed: array<int>})
  type t = base
  type subst = Map.t<int, t>
  type gen = ref<int>
  type typeTag<_> += Tag: typeTag<t>
  let tag = Tag
  let tagEq = (type a, tag: typeTag<a>): option<eq<t, a>> =>
    switch tag {
    | Tag => Some(Refl)
    | _ => None
    }
  let coercions = {
    let left =
      Left.coercions->Array.map((Coercion((tag, f))) => Coercion((
        tag,
        x => f(x)->Option.map(v => Left(v)),
      )))
    let right =
      Right.coercions->Array.map((Coercion((tag, f))) => Coercion((
        tag,
        x => f(x)->Option.map(v => Right(v)),
      )))
    Array.concat(left, right)
  }
  let match = (t, leftBranch: Left.t => 'a, rightBranch: Right.t => 'a): 'a =>
    switch t {
    | Left(s) => leftBranch(s)
    | Right(s) => rightBranch(s)
    | Var(_) => throw(MatchCombineAtomVar)
    | Schematic(_) => throw(MatchCombineAtomSchematic)
    }
  let parse = (s, ~scope, ~gen: option<gen>=?) => {
    Left.parse(s, ~scope, ~gen?)
    ->Result.map(((r, rest)) => (Left(r), rest))
    ->Util.Result.or(() =>
      Right.parse(s, ~scope, ~gen?)->Result.map(((r, rest)) => (Right(r), rest))
    )
  }
  let prettyPrint = (s, ~scope) =>
    s->match(left => Left.prettyPrint(left, ~scope), right => Right.prettyPrint(right, ~scope))
  let unify = (s1, s2, ~gen=?) =>
    switch (s1, s2) {
    | (Left(s1), Left(s2)) =>
      Left.unify(s1, s2, ~gen?)->Seq.map(subst => subst->Util.mapMapValues(v => Left(v)))
    | (Right(s1), Right(s2)) =>
      Right.unify(s1, s2, ~gen?)->Seq.map(subst => subst->Util.mapMapValues(v => Right(v)))
    | (_, _) => Seq.empty
    }
  let coerceLeft = (left: Left.t): option<Right.t> =>
    Array.findMap(Right.coercions, (Coercion((tag, f))) =>
      switch Left.tagEq(tag) {
      | Some(Refl) => f(left)
      | None => None
      }
    )
  let coerceRight = (right: Right.t): option<Left.t> => {
    Array.findMap(Left.coercions, (Coercion((tag, f))) =>
      switch Right.tagEq(tag) {
      | Some(Refl) => f(right)
      | None => None
      }
    )
  }
  let substitute = (s, subst: subst) => {
    s->match(
      left => {
        let leftSubs =
          subst->Util.Map.filterMap((_, v) => v->match(left => Some(left), coerceRight))
        Left(Left.substitute(left, leftSubs))
      },
      right => {
        let rightSubs =
          subst->Util.Map.filterMap((_, v) => v->match(coerceLeft, right => Some(right)))
        Right(Right.substitute(right, rightSubs))
      },
    )
  }
  let upshift = (s, amount: int, ~from=?) =>
    s->match(
      left => Left(left->Left.upshift(amount, ~from?)),
      right => Right(right->Right.upshift(amount, ~from?)),
    )
  let lowerVar = idx => Some(Var({idx: idx}))
  let lowerSchematic = (schematic, allowed) => Some(Schematic({schematic, allowed}))
  let substDeBruijn = (s, substs: array<option<t>>, ~from=?) =>
    s->match(
      left => {
        let leftSubs = substs->Array.map(o =>
          o->Option.flatMap(s =>
            switch s {
            | Left(s) => Some(s)
            | Var({idx}) => Left.lowerVar(idx)
            | Schematic({schematic, allowed}) => Left.lowerSchematic(schematic, allowed)
            | Right(s) => coerceRight(s)
            }
          )
        )
        Left(Left.substDeBruijn(left, leftSubs, ~from?))
      },
      right => {
        let rightSubs = substs->Array.map(o =>
          o->Option.flatMap(s =>
            switch s {
            | Right(s) => Some(s)
            | Var({idx}) => Right.lowerVar(idx)
            | Schematic({schematic, allowed}) => Right.lowerSchematic(schematic, allowed)
            | Left(s) => coerceLeft(s)
            }
          )
        )
        Right(Right.substDeBruijn(right, rightSubs, ~from?))
      },
    )
  let concrete = s => s->match(Left.concrete, Right.concrete)
}

module type ATOM_VIEW = {
  module Atom: ATOM
  type props = {name: Atom.t, scope: array<string>}
  let make: props => React.element
}

module MakeAtomView = (
  Left: COERCIBLE_ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: COERCIBLE_ATOM,
  RightView: ATOM_VIEW with module Atom := Right,
  Combined: module type of CombineAtom(Left, Right),
): {
  include ATOM_VIEW with module Atom := Combined
} => {
  type props = {name: Combined.t, scope: array<string>}
  let make = ({name, scope}: props) =>
    name->Combined.match(
      left => <LeftView name={left} scope />,
      right => <RightView name={right} scope />,
    )
}

module MakeAtomAndView = (
  Left: COERCIBLE_ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: COERCIBLE_ATOM,
  RightView: ATOM_VIEW with module Atom := Right,
) => {
  module Atom = CombineAtom(Left, Right)
  module AtomView = MakeAtomView(Left, LeftView, Right, RightView, Atom)
}
