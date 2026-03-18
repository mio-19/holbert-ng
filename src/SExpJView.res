module SExpJ = SExp
module TermView = SExpView
type props = {
  judgment: SExpJ.t,
  scope: array<string>,
}
let make = ({judgment, scope}) => SExpView.make({term: judgment, scope})
