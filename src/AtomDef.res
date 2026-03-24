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

type rec existsValue = ExistsValue(typeTag<'a>, 'a): existsValue
module type COERCIBLE_ATOM = {
  include ATOM
  let liftForeign: existsValue => option<t>
  let unwrap: t => existsValue
}

// coercion<t> represents a coercion from some type 'a to t,
// along with a function that effectively checks whether its argument is
// an instance of the coerced type. see usage in MakeCoercible.liftForeign
type rec coercion<_> =
  Coercion({tagEq: 'c. typeTag<'c> => option<eq<'a, 'c>>, coerce: 'a => option<'b>}): coercion<'b>
module MakeCoercible = (
  Atom: ATOM,
  Coercions: {
    let coercions: array<coercion<Atom.t>>
  },
): (COERCIBLE_ATOM with type t = Atom.t) => {
  include Atom
  let liftForeign = (ExistsValue(tag, val)) =>
    Array.findMap(Coercions.coercions, (Coercion(c)) =>
      switch c.tagEq(tag) {
      | Some(Refl) => c.coerce(val)
      | None => None
      }
    )
  let unwrap = t => ExistsValue(Atom.tag, t)
}

exception MatchCombineAtomBoth
exception MatchCombineAtomForeign

module CombineAtom = (Left: COERCIBLE_ATOM, Right: COERCIBLE_ATOM): {
  type rec base =
    | Left(Left.t)
    | Right(Right.t)
    // used strictly for SExp -> Atom coercions
    // should not be parsed or otherwise appear organically
    | Both(option<Left.t>, option<Right.t>)
    // likewise, strictly for Atom <-> Atom coercions
    // occurs only when passed from some upper part of the tree
    | Foreign(existsValue)
  include COERCIBLE_ATOM with type t = base
  let match: (t, Left.t => 'a, Right.t => 'a) => 'a
} => {
  type rec base =
    | Left(Left.t)
    | Right(Right.t)
    | Both(option<Left.t>, option<Right.t>)
    | Foreign(existsValue)
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
  let liftForeign = v => Some(Foreign(v))
  let match = (t, leftBranch: Left.t => 'a, rightBranch: Right.t => 'a): 'a =>
    switch t {
    | Left(s) => leftBranch(s)
    | Right(s) => rightBranch(s)
    | Both(_) => throw(MatchCombineAtomBoth)
    | Foreign(_) => throw(MatchCombineAtomForeign)
    }
  let unwrap = t => t->match(Left.unwrap, Right.unwrap)
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
  let coerceToLeft = (t): option<Left.t> =>
    switch t {
    | Left(s) => Some(s)
    | Both(os, _) => os
    | Right(s) => s->Right.unwrap->Left.liftForeign
    | Foreign(v) => Left.liftForeign(v)
    }
  let coerceToRight = (t): option<Right.t> =>
    switch t {
    | Right(s) => Some(s)
    | Both(_, os) => os
    | Left(s) => s->Left.unwrap->Right.liftForeign
    | Foreign(v) => Right.liftForeign(v)
    }
  let substitute = (s, subst: subst) => {
    s->match(
      left => {
        let leftSubs = subst->Util.Map.filterMap((_, v) => coerceToLeft(v))
        Left(Left.substitute(left, leftSubs))
      },
      right => {
        let rightSubs = subst->Util.Map.filterMap((_, v) => coerceToRight(v))
        Right(Right.substitute(right, rightSubs))
      },
    )
  }
  let upshift = (s, amount: int, ~from=?) =>
    s->match(
      left => Left(left->Left.upshift(amount, ~from?)),
      right => Right(right->Right.upshift(amount, ~from?)),
    )
  let lowerVar = idx => Some(Both(Left.lowerVar(idx), Right.lowerVar(idx)))
  let lowerSchematic = (schematic, allowed) => Some(
    Both(Left.lowerSchematic(schematic, allowed), Right.lowerSchematic(schematic, allowed)),
  )
  let substDeBruijn = (s, substs: array<option<t>>, ~from=?) =>
    s->match(
      left => {
        let leftSubs = substs->Array.map(o => o->Option.flatMap(coerceToLeft))
        Left(Left.substDeBruijn(left, leftSubs, ~from?))
      },
      right => {
        let rightSubs = substs->Array.map(o => o->Option.flatMap(coerceToRight))
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
