module SExpJ = JudgmentFunctor.SExpJ
module AxiomS = Editable.TextArea(AxiomSet.Make(SExp, SExpJ, SExpJView))
module DerivationsOrLemmasView = MethodView.CombineMethodView(
  SExp,
  SExpJ,
  MethodView.DerivationView(SExp, SExpJ),
  MethodView.LemmaView(SExp, SExpJ, SExpJView),
)
module TheoremS = Editable.TextArea(Theorem.Make(SExp, SExpJ, SExpJView, DerivationsOrLemmasView))
module ConfS = ConfigBlock.Make(SExp, SExpJ)

module AxiomStr = Editable.TextArea(StringAxiomSet)
module DerivationsOrLemmasStrView = MethodView.CombineMethodView(
  StringTerm,
  StringTermJudgment,
  MethodView.DerivationView(StringTerm, StringTermJudgment),
  MethodView.LemmaView(StringTerm, StringTermJudgment, StringTermJView),
)
module DLEStrView = MethodView.CombineMethodView(
  StringTerm,
  StringTermJudgment,
  DerivationsOrLemmasStrView,
  MethodView.EliminationView(StringTerm, StringTermJudgment),
)
module TheoremStr = Editable.TextArea(
  Theorem.Make(StringTerm, StringTermJudgment, StringTermJView, DerivationsOrLemmasStrView),
)
