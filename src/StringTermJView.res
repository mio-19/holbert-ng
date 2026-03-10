open StringTermJudgment

module StringSymbolView: SExpViewFunc.SYMBOL_VIEW with module Symbol := StringSymbol = {
  type props = {name: StringSymbol.t, scope: array<string>}
  let make = ({name, scope}: props) =>
    switch name {
    | StringS(term) => <StringTermView term scope />
    | ConstS(name) => <SExpView.ConstSymbolView name scope />
    }
}

module TermView = StringTermView
type props = {
  judgment: StringTermJudgment.t,
  scope: array<string>,
}
module SExpView = SExpViewFunc.Make(
  StringTermJudgment.StringSymbol,
  StringSymbolView,
  StringTermJudgment.StringSExp,
)
let make = ({judgment: (term, j), scope}) => {
  <span className="term-compound">
    <StringTermView term scope />
    {React.string(" ")}
    <SExpView term=j scope />
  </span>
}
