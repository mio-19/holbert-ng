type t = string
module Atom: AtomDef.ATOM with type t = t
module AtomView: SExpViewFunc.ATOM_VIEW with module Atom := Atom
