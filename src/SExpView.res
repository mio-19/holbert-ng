module ConstSymbol = SExp.Symbol
module ConstSymbolView: SExpViewFunc.SYMBOL_VIEW with module Symbol := SExp.Symbol = {
  type props = {name: string, scope: array<string>}
  let make = (props: props) => React.string(props.name)
}

include SExpViewFunc.Make(ConstSymbol, ConstSymbolView, SExp)
