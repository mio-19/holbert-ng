module StringSymbol: SExpFunc.SYMBOL
module StringSExp: module type of SExpFunc.Make(StringSymbol)

type t = (StringTerm.t, StringSExp.t)

include Signatures.JUDGMENT with module Term := StringTerm and type t := t
