open AtomDef

module StringA: COERCIBLE_ATOM with type t = StringA.t = {
  include StringA.Atom
  let coerce = (HValue(tag, a)) =>
    switch tag {
    | Symbolic.Atom.Tag => Some([StringA.String(a)])
    | AtomDef.SExpTag =>
      Some([
        switch a {
        | Var({idx}) => StringA.Var({idx: idx})
        | Schematic({schematic, allowed}) => StringA.Schematic({schematic, allowed})
        },
      ])
    | _ => None
    }
  let wrap = a => HValue(StringA.Atom.Tag, a)
}

module Symbolic: COERCIBLE_ATOM with type t = Symbolic.Atom.t = {
  include Symbolic.Atom
  let coerce = _ => None
  let wrap = a => HValue(Symbolic.Atom.Tag, a)
}
