open AtomDef

module StringA = MakeCoercible(
  StringA.Atom,
  {
    let coercions = [Coercion((Symbolic.Atom.tag, s => Some([StringA.String(s)])))]
  },
)

module Symbolic = MakeCoercible(
  Symbolic.Atom,
  {
    let coercions = []
  },
)
