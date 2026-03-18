module SymbolAtom: SExpFunc.ATOM with type t = string

include module type of SExpFunc.Make(SymbolAtom)
