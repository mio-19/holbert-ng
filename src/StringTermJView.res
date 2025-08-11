module TermView = StringTermView
type props = {
  judgment: StringTermJudgment.t,
  scope: array<string>,
}
let make = ({judgment, scope}) => {
  let (term, m) = StringTermJudgment.get(judgment)
  <span className="term-compound">
    <StringTermView term scope />
    {React.string(` ${m}`)}
  </span>
}
