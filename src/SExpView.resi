module ConstSymbolView: SExpViewFunc.SYMBOL_VIEW with module Symbol := SExp.Symbol

include Signatures.TERM_VIEW with module Term := SExp
