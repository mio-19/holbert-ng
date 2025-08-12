type t = (StringTerm.t, string, SExp.t)

include Signatures.JUDGMENT with module Term := StringTerm and type t := t
