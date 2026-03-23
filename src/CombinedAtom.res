module type ATOM = SExpFunc.ATOM
exception RawVarOrSchematic

module MakeAtom = (Left: ATOM, Right: ATOM): {
  type base =
    | Left(Left.t)
    | Right(Right.t)
    // neither of the below should appear organically. they're purely so we can lower
    // substitutions to both Left.t and Right.t
    | Var({idx: int})
    | Schematic({schematic: int, allowed: array<int>})
  include ATOM with type t = base
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
  let match = (t, leftBranch: Left.t => 'a, rightBranch: Right.t => 'a): 'a =>
    switch t {
    | Left(s) => leftBranch(s)
    | Right(s) => rightBranch(s)
    | _ => throw(RawVarOrSchematic)
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
  let substitute = (s, subst: subst) => {
    s->match(
      left => {
        let leftSubs = subst->Util.Map.filterMap((_, v) =>
          switch v {
          | Left(s) => Some(s)
          | _ => None
          }
        )
        Left(Left.substitute(left, leftSubs))
      },
      right => {
        let rightSubs = subst->Util.Map.filterMap((_, v) =>
          switch v {
          | Right(s) => Some(s)
          | _ => None
          }
        )
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
        let leftSubs = substs->Array.map(s =>
          switch s {
          | Some(Left(s)) => Some(s)
          | Some(Var({idx})) => Left.lowerVar(idx)
          | Some(Schematic({schematic, allowed})) => Left.lowerSchematic(schematic, allowed)
          | _ => None
          }
        )
        Left(Left.substDeBruijn(left, leftSubs, ~from?))
      },
      right => {
        let rightSubs = substs->Array.map(s =>
          switch s {
          | Some(Right(s)) => Some(s)
          | Some(Var({idx})) => Right.lowerVar(idx)
          | Some(Schematic({schematic, allowed})) => Right.lowerSchematic(schematic, allowed)
          | _ => None
          }
        )
        Right(Right.substDeBruijn(right, rightSubs, ~from?))
      },
    )
  let concrete = s => s->match(Left.concrete, Right.concrete)
}

module type ATOM_VIEW = SExpViewFunc.ATOM_VIEW
module MakeAtomView = (
  Left: ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: ATOM,
  RightView: ATOM_VIEW with module Atom := Right,
  Combined: module type of MakeAtom(Left, Right),
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
  Left: ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: ATOM,
  RightView: ATOM_VIEW with module Atom := Right,
) => {
  module Atom = MakeAtom(Left, Right)
  module AtomView = MakeAtomView(Left, LeftView, Right, RightView, Atom)
}
