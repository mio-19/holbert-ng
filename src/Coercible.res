open AtomDef

module StringA = MakeCoercible(
  StringA.Atom,
  {
    let coercions = [
      Coercion({
        tagEq: Symbolic.Atom.tagEq,
        coerce: s => Some([StringA.String(s)]),
      }),
    ]
  },
)

module Symbolic = MakeCoercible(
  Symbolic.Atom,
  {
    let coercions = []
  },
)
