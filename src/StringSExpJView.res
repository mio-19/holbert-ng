module StringAtomView: SExpViewFunc.ATOM_VIEW with module Atom := StringSExp.StringSExpAtom = {
  type props = {name: StringSExp.StringSExpAtom.t, scope: array<string>}
  let make = ({name, scope}: props) =>
    switch name {
    | StringSExp.StringS(name) => <StringAtomView name scope />
    | StringSExp.ConstS(name) => <SExpView.SymbolAtomView name scope />
    }
}

module StringSExpJ = StringSExp
module View = SExpViewFunc.Make(StringSExp.StringSExpAtom, StringAtomView, StringSExp)

module TermView = View
type props = {
  judgment: StringSExp.t,
  scope: array<string>,
}
let make = ({judgment, scope}) => View.make({term: judgment, scope})
