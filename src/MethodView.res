open Signatures
open Method
module type METHOD_VIEW = {
  module Term: TERM
  module Judgment: JUDGMENT with module Term := Term
  module Method: PROOF_METHOD with module Term := Term and module Judgment := Judgment
  type props<'a> = {method: Method.t<'a>, scope: array<Term.meta>, ruleStyle: RuleView.style}
  type srProps<'a> = {"proof": 'a, "scope": array<Term.meta>, "ruleStyle": RuleView.style}
  let make: (srProps<'a> => React.element) => props<'a> => React.element
}

module DerivationView = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Method = Derivation(Term, Judgment)
  type props<'a> = {method: Method.t<'a>, scope: array<Term.meta>, ruleStyle: RuleView.style}
  type srProps<'a> = {"proof": 'a, "scope": array<Term.meta>, "ruleStyle": RuleView.style}
  let make = (subRender: srProps<'a> => React.element) => props => {
    <div>
      <b> {React.string("by ")} </b>
      {React.string(props.method.ruleName)}
      <ul>
        {props.method.subgoals
        ->Array.mapWithIndex((sg, i) => {
          <li key={String.make(i)}>
            {React.createElement(
              subRender,
              {"proof": sg, "scope": props.scope, "ruleStyle": props.ruleStyle},
            )}
          </li>
        })
        ->React.array}
      </ul>
    </div>
  }
}

module LemmaView = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Method = Lemma(Term, Judgment)
  type props<'a> = {method: Method.t<'a>, scope: array<Term.meta>, ruleStyle: RuleView.style}
  type srProps<'a> = {"proof": 'a, "scope": array<Term.meta>, "ruleStyle": RuleView.style}
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  let make = (subRender: srProps<'a> => React.element) => props => {
    <div>
      <b> {React.string("have ")} </b>
      <RuleView rule={props.method.rule} scope={props.scope} style={props.ruleStyle}>
        {React.null}
      </RuleView>
      {React.createElement(
        subRender,
        {"proof": props.method.proof, "scope": props.scope, "ruleStyle": props.ruleStyle},
      )}
      {React.createElement(
        subRender,
        {"proof": props.method.show, "scope": props.scope, "ruleStyle": props.ruleStyle},
      )}
    </div>
  }
}
module CombineMethodView = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  Method1View: METHOD_VIEW with module Term := Term and module Judgment := Judgment,
  Method2View: METHOD_VIEW
    with module Term := Term
    and module Judgment := Judgment
    and type srProps<'a> = Method1View.srProps<'a>,
) => {
  module Method = Combine(Term, Judgment, Method1View.Method, Method2View.Method)
  type props<'a> = {method: Method.t<'a>, scope: array<Term.meta>, ruleStyle: RuleView.style}
  type srProps<'a> = Method1View.srProps<'a>
  let make = (subrender: srProps<'a> => React.element) => props => {
    switch props.method {
    | First(m) =>
      Method1View.make(subrender)({method: m, scope: props.scope, ruleStyle: props.ruleStyle})
    | Second(m) =>
      Method2View.make(subrender)({method: m, scope: props.scope, ruleStyle: props.ruleStyle})
    }
  }
}
