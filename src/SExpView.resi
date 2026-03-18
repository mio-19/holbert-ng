module SymbolAtomView: SExpViewFunc.ATOM_VIEW with module Atom := SExp.SymbolAtom

include Signatures.TERM_VIEW with module Term := SExp
