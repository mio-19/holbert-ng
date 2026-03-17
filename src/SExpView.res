module ConstAtom = SExp.Atom
module ConstAtomView: SExpViewFunc.ATOM_VIEW with module Atom := SExp.Atom = {
  type props = {name: string, scope: array<string>}
  let make = (props: props) => React.string(props.name)
}

include SExpViewFunc.Make(ConstAtom, ConstAtomView, SExp)
