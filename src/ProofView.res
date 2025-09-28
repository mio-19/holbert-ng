open Signatures
open MethodView

module Make = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  MethodView: METHOD_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module ScopeView = ScopeView.Make(Term, JudgmentView.TermView)
  module Proof = Proof.Make(Term, Judgment, MethodView.Method)

  type props = {
    proof: Proof.checked,
    scope: array<Term.meta>,
    ruleStyle: RuleView.style,
  }
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  @react.componentWithProps
  let rec make = (props: props) => {
    switch props.proof {
    | Proof.Checked({fixes, assumptions, method, rule}) => {
        let scope = Array.concat(fixes, props.scope)
        <>
          <ScopeView scope=fixes />
          <ul className="proof-assumptions">
            {Belt.Array.zipBy(assumptions, rule.premises, (n, r) => {
              <li key={n}>
                <RuleView rule=r style=props.ruleStyle scope> {React.string(n)} </RuleView>
              </li>
            })->React.array}
          </ul>
          <div className="proof-show">
            <JudgmentView judgment={rule.conclusion} scope />
            {switch method {
            | Goal(options) =>
              <button onClick={_ => Console.log(options(Term.makeGen()))}>
                {React.string("Test")}
              </button>
            | Do(method) =>
              React.createElement(
                MethodView.make(p =>
                  make({proof: p["proof"], scope: p["scope"], ruleStyle: p["ruleStyle"]})
                ),
                {method, scope, ruleStyle: props.ruleStyle},
              )
            }}
          </div>
        </>
      }
    | Proof.ProofError({raw: _, rule: _, msg}) => <div className="error"> {React.string(msg)} </div>
    }
  }
}
