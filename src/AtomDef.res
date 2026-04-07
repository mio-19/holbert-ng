// type level stuff to enable well-typed coercions
type atomTag<_> = ..

module type ATOM = {
  type t
  type subst = Map.t<int, t>
  type atomTag<_> += Tag: atomTag<t>
  let unify: (t, t, ~gen: ref<int>=?) => Seq.t<subst>
  let prettyPrint: (t, ~scope: array<string>) => string
  let parse: (string, ~scope: array<string>, ~gen: ref<int>=?) => result<(t, string), string>
  let substitute: (t, subst) => t
  let upshift: (t, int, ~from: int=?) => t
  let substDeBruijn: (t, array<option<t>>, ~from: int=?) => t
  let concrete: t => bool
}

type rec hValue = HValue(atomTag<'a>, 'a): hValue
module type COERCIBLE_ATOM = {
  include ATOM
  let coerce: hValue => option<t>
  let wrap: t => hValue
}

type loweredSExp = Var({idx: int}) | Schematic({schematic: int, allowed: array<int>})
type atomTag<_> += SExpTag: atomTag<loweredSExp>

exception MatchCombineAtomForeign

module CombineAtom = (Left: COERCIBLE_ATOM, Right: COERCIBLE_ATOM): {
  type rec base =
    | Left(Left.t)
    | Right(Right.t)
    // strictly for coercions
    // occurs when passed from some relative in the tree
    // or when SExp values are lowered into loweredSExp
    | Foreign(hValue)
  include COERCIBLE_ATOM with type t = base
  let match: (t, Left.t => 'a, Right.t => 'a) => 'a
} => {
  type rec base =
    | Left(Left.t)
    | Right(Right.t)
    | Foreign(hValue)
  type t = base
  type subst = Map.t<int, t>
  type gen = ref<int>
  type atomTag<_> += Tag: atomTag<t>
  let coerce = v => Some(Foreign(v))
  let match = (t, leftBranch: Left.t => 'a, rightBranch: Right.t => 'a): 'a =>
    switch t {
    | Left(s) => leftBranch(s)
    | Right(s) => rightBranch(s)
    | Foreign(_) => throw(MatchCombineAtomForeign)
    }
  let wrap = t =>
    switch t {
    | Left(s) => Left.wrap(s)
    | Right(s) => Right.wrap(s)
    | Foreign(val) => val
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
  let coerceToLeft = (t): option<Left.t> =>
    switch t {
    | Left(s) => Some(s)
    | Right(s) => Right.wrap(s)->Left.coerce
    | Foreign(v) => Left.coerce(v)
    }
  let coerceToRight = (t): option<Right.t> =>
    switch t {
    | Right(s) => Some(s)
    | Left(s) => Left.wrap(s)->Right.coerce
    | Foreign(v) => Right.coerce(v)
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
