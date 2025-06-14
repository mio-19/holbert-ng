open Editable
open ProofEngine
module RuleSExpTE = RuleSetSB(SExp,SExp,SExpJView)
module RuleSExpView = WithTextArea(RuleSExpTE)
include RuleSExpView//(RuleSExpView.Hypothetical(RuleSExpView.Inline))
module PM = Proof(SExp,SExp,Combine(SExp,SExp,Derivation(SExp,SExp),Lemma(SExp,SExp)))


//include SExpBaseView

