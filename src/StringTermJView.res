module StringSymbolView: SExpViewFunc.SYMBOL_VIEW
  with module Symbol := StringTermJudgment.StringSymbol = {
  type props = {name: StringTermJudgment.StringSymbol.t, scope: array<string>}
  let make = ({name, scope}: props) =>
    switch name {
    | StringTermJudgment.StringS(term) => <StringTermView term scope />
    | StringTermJudgment.ConstS(name) => <SExpView.ConstSymbolView name scope />
    }
}

module View = SExpViewFunc.Make(
  StringTermJudgment.StringSymbol,
  StringSymbolView,
  StringTermJudgment.StringSExp,
)

module TermView = View
type props = {
  judgment: StringTermJudgment.StringSExp.t,
  scope: array<string>,
}
let make = ({judgment, scope}) => View.make({term: judgment, scope})
