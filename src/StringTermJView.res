module TermView = StringTermView
type props = {
  judgment: StringTermJudgment.t,
  scope: array<string>,
}
module SExpView = SExpViewFunc.Make(StringTermJudgment.StringSymbol, StringTermJudgment.StringSExp)
let make = ({judgment: (term, j), scope}) => {
  <span className="term-compound">
    <StringTermView term scope />
    {React.string(" ")}
    <SExpView term=j scope />
  </span>
}
