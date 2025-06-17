module AxiomS = Editable.TextArea(AxiomSet.Make(SExp,SExp,SExpJView))
module DerivationsOrLemmas = Method.Combine(SExp,SExp,Method.Derivation(SExp,SExp),Method.Lemma(SExp,SExp))
module TheoremS = Editable.TextArea(Theorem.Make(SExp,SExp,SExpJView,DerivationsOrLemmas))
module ConfS = ConfigBlock.Make(SExp,SExp)

