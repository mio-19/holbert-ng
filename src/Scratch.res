open Editable
module RuleSExpTE = RuleSetSB(SExp,SExp,SExpJView)
module RuleSExpView = WithTextArea(RuleSExpTE)
include RuleSExpView//(RuleSExpView.Hypothetical(RuleSExpView.Inline))
//include SExpBaseView

