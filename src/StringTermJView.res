module StringAtomView: SExpViewFunc.ATOM_VIEW with module Atom := StringSExp.StringAtom = {
  type props = {name: StringSExp.StringAtom.t, scope: array<string>}
  let make = ({name, scope}: props) =>
    switch name {
    | StringSExp.StringS(term) => <StringTermView term scope />
    | StringSExp.ConstS(name) => <SExpView.ConstAtomView name scope />
    }
}

module View = SExpViewFunc.Make(StringSExp.StringAtom, StringAtomView, StringSExp)

module TermView = View
type props = {
  judgment: StringSExp.t,
  scope: array<string>,
}
let make = ({judgment, scope}) => View.make({term: judgment, scope})
