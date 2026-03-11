type stringSymbol = StringS(StringTerm.t) | ConstS(SExp.ConstSymbol.t)
module StringSymbol: SExpFunc.SYMBOL with type t = stringSymbol

module StringSExp: module type of SExpFunc.Make(StringSymbol)
module StringSExpJ: module type of TermAsJudgment.Make(StringSExp)
type t = StringSExpJ.t
let constSymbol: string => StringSExp.t

include Signatures.JUDGMENT with module Term := StringSExp and type t := t
