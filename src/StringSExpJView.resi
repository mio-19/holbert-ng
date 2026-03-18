module StringSExpJ: Signatures.JUDGMENT with module Term := StringSExp and type t = StringSExp.t

include Signatures.JUDGMENT_VIEW with module Term := StringSExp and module Judgment := StringSExpJ
