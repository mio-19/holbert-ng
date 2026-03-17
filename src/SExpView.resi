module ConstAtomView: SExpViewFunc.ATOM_VIEW with module Atom := SExp.Atom

include Signatures.TERM_VIEW with module Term := SExp
