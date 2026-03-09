type stringSymbol = StringS(StringTerm.t) | ConstS(SExp.ConstSymbol.t)
module StringSymbol: SExpFunc.SYMBOL with type t = stringSymbol

module StringSExp: module type of SExpFunc.Make(StringSymbol)

type t = (StringTerm.t, StringSExp.t)
let constSymbol: string => StringSExp.t

include Signatures.JUDGMENT with module Term := StringTerm and type t := t
