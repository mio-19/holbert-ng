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
include SExpViewFunc.Make(
  StringTermJudgment.StringSymbol,
  StringSymbolView,
  StringTermJudgment.StringSExp,
)
