type stringSymbol = StringS(StringTerm.t) | ConstS(SExp.Symbol.t)
module StringSymbol: SExpFunc.SYMBOL with type t = stringSymbol

type rec t =
  | Symbol(StringSymbol.t)
  | Compound({subexps: array<t>})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
  | Ghost

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Map.t<int, t>

module Symbol: SExpFunc.SYMBOL with type t := StringSymbol.t
let mapTerms: (t, t => t) => t
