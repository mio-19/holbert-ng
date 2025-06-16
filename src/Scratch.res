open Editable
open ProofEngine
module RuleSExpTE = RuleSetSB(SExp,SExp,SExpJView)
module RuleSExpView = WithTextArea(RuleSExpTE)
include RuleSExpView//(RuleSExpView.Hypothetical(RuleSExpView.Inline))
module Method = Combine(SExp,SExp,Derivation(SExp,SExp),Lemma(SExp,SExp))
module PM = Proof(SExp,SExp,Method)
module TheoremTE = TheoremSB(SExp,SExp,SExpJView, Method)
module TheoremView = WithTextArea(TheoremTE)

let \"Theorem" = TheoremView.make

//include SExpBaseView

