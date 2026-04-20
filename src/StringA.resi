type rec piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type t = array<piece>

module BaseAtom: AtomDef.BASE_ATOM with type t = t
module Atom: AtomDef.ATOM with module BaseAtom = BaseAtom
module AtomView: AtomDef.ATOM_VIEW with module Atom := Atom
