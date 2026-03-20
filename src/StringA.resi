type rec piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type t = array<piece>

module Atom: SExpFunc.ATOM with type t = t
module AtomView: SExpViewFunc.ATOM_VIEW with module Atom := Atom
