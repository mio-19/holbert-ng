// really feel like this shouldn't be necessary...
module SExpJ: Signatures.JUDGMENT with module Term := SExp and type t = SExp.t

include Signatures.JUDGMENT_VIEW with module Term := SExp and module Judgment := SExpJ
