module SymbolAtomView: SExpViewFunc.ATOM_VIEW with module Atom := SExp.SymbolAtom = {
  type props = {name: string, scope: array<string>}
  let make = (props: props) => React.string(props.name)
}

include SExpViewFunc.Make(SExp.SymbolAtom, SymbolAtomView, SExp)
