module TermView = SExpView
module SExpJ = JudgmentFunctor.SExpJ
type props = {
  judgment: SExp.t,
  scope: array<string>,
}
let make = ({judgment, scope}) => SExpView.make({term: judgment, scope})
