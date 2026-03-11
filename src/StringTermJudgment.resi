type stringSymbol = StringS(StringTerm.t) | ConstS(SExp.ConstSymbol.t)
module StringSymbol: SExpFunc.SYMBOL with type t = stringSymbol

module StringSExp: module type of SExpFunc.Make(StringSymbol)
type t = StringSExp.t

include Signatures.JUDGMENT with module Term := StringSExp and type t := t
