module ConstSymbol: SExpFunc.SYMBOL

include module type of SExpFunc.Make(ConstSymbol)
let symbol: string => t
let getSymbol: ConstSymbol.t => string
