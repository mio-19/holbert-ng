module HOTermJ = TermAsJudgment.HOTermJ

module AxiomS = Editable.TextArea(AxiomSet.Make(HOTerm, HOTermJ, HOTermJView))
module InductiveS = Editable.TextArea(InductiveSet.Make(HOTerm, HOTermJ, HOTermJView))

module RewritesView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  MethodView.RewriteView(HOTermJ),
  MethodView.RewriteReverseView(HOTermJ),
)
module DerivationsOrLemmasView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  MethodView.CombineMethodView(
    HOTerm,
    HOTermJ,
    MethodView.DerivationView(HOTerm, HOTermJ),
    MethodView.LemmaView(HOTerm, HOTermJ, HOTermJView),
  ),
  MethodView.EliminationView(HOTerm, HOTermJ),
)
module DLRView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  DerivationsOrLemmasView,
  RewritesView,
)
module DLREView = MethodView.CombineMethodView(
  HOTerm,
  HOTermJ,
  DLRView,
  MethodView.EliminationView(HOTerm, HOTermJ),
)

// Temporarily use DLRView (without Elimination) due to HOTerm unification bug
module TheoremS = Editable.TextArea(Theorem.Make(HOTerm, HOTermJ, HOTermJView, DLRView))
module ConfS = ConfigBlock.Make(HOTerm, HOTermJ)

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
  Theorem.Make(StringTerm, StringTermJudgment, StringTermJView, DLEStrView),
)
