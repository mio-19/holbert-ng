module TermView = StringTermView
type props = {
  judgment: StringTermJudgment.t,
  scope: array<string>,
}
let make = ({judgment: (term, m, j), scope}) => {
  <span className="term-compound">
    <StringTermView term scope />
    {React.string(` ${m} `)}
    <SExpView term=j scope={[]} />
  </span>
}
