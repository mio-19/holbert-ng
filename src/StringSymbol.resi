type t = StringS(StringA.Atom.t) | ConstS(string)
module Atom: SExpFunc.ATOM with type t = t
module AtomView: SExpViewFunc.ATOM_VIEW with module Atom := Atom
