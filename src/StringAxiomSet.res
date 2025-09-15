open Signatures
open Component

module Term = StringTerm
module Judgment = StringTermJudgment
module JudgmentView = StringTermJView
module BaseSet = AxiomSet.Make(Term, Judgment, JudgmentView)
module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
module Ports = BaseSet.Ports
type props = BaseSet.props

let deserialise = (str: string) => {
  let base = BaseSet.deserialise(str)
  base
}

let make: BaseSet.props => Jsx.element = props => {
  BaseSet.make(props)
}
