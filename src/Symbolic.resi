module BaseAtom: AtomDef.BASE_ATOM with type t = string
module Atom: AtomDef.ATOM with module BaseAtom = BaseAtom
module AtomView: AtomDef.ATOM_VIEW with module Atom := Atom
