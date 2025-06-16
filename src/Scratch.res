open Editable
open Method
open Util
module AxiomS = AxiomSet(SExp,SExp,SExpJView)
module DerivationsOrLemmas = Combine(SExp,SExp,Derivation(SExp,SExp),Lemma(SExp,SExp))
module TheoremS = Theorem(SExp,SExp,SExpJView,DerivationsOrLemmas)
module Conf = Config(SExp,SExp)
//module RuleSExpTE = RuleSetSB(SExp,SExp,SExpJView)
//module RuleSExpView = WithTextArea(RuleSExpTE)
//include RuleSExpView//(RuleSExpView.Hypothetical(RuleSExpView.Inline))
//
//module PM = Proof.Make(SExp,SExp,DerivationsOrLemmas)

//module TheoremView = WithTextArea(TheoremTE)

let \"AxiomSet" = AxiomS.make

//include SExpBaseView

