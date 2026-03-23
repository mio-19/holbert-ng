module type ATOM = SExpFunc.ATOM
exception RawVarOrSchematic

module MakeAtom = (Left: AtomDef.COERCIBLE_ATOM, Right: AtomDef.COERCIBLE_ATOM): {
  type base =
    | Left(Left.t)
    | Right(Right.t)
    // neither of the below should appear organically. they're purely so we can lower
    // substitutions to both Left.t and Right.t
    | Var({idx: int})
    | Schematic({schematic: int, allowed: array<int>})
  include ATOM with type t = base
  let match: (t, Left.t => 'a, Right.t => 'a) => 'a
} => AtomDef.CombineAtom(Left, Right)

module type ATOM_VIEW = SExpViewFunc.ATOM_VIEW
module MakeAtomView = (
  Left: AtomDef.COERCIBLE_ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: AtomDef.COERCIBLE_ATOM,
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
  Left: AtomDef.COERCIBLE_ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: AtomDef.COERCIBLE_ATOM,
  RightView: ATOM_VIEW with module Atom := Right,
) => {
  module Atom = MakeAtom(Left, Right)
  module AtomView = MakeAtomView(Left, LeftView, Right, RightView, Atom)
}
