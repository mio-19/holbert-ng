open Signatures
open Component
open MethodView
module Make = (
  Term: TERM,
  Judgment: JUDGMENT with module Term := Term,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  MethodView: METHOD_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module Proof = Proof.Make(Term, Judgment, MethodView.Method)
  module Context = Method.Context(Term, Judgment)
  module ProofView = ProofView.Make(Term, Judgment, JudgmentView, MethodView)
  open RuleView
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  module Ports = Ports(Term, Judgment)
  type state = {name: string, rule: Rule.t, proof: Proof.t}
  type props = {
    content: state,
    imports: Ports.t,
    onChange: (state, ~exports: Ports.t) => unit,
  }
  let serialise = (state: state) => {
    state.rule
    ->Rule.prettyPrintTopLevel(~name=state.name)
    ->String.concat(Proof.prettyPrint(state.proof, ~scope=[]))
  }
  let deserialise = (str: string, ~imports: Ports.t) => {
    let _facts = imports.facts
    let gen = Term.makeGen()
    let cur = ref(str)
    switch Rule.parseTopLevel(cur.contents, ~scope=[], ~gen) {
    | Error(e) => Error(e)
    | Ok(((rule, name), s)) =>
      switch Proof.parse(s, ~scope=[], ~gen) {
      | Error(e) => Error(e)
      | Ok((_, s')) if String.length(String.trim(s')) > 0 =>
        Error("Trailing input: "->String.concat(s'))
      | Ok((proof, _)) => Ok((
          {name, rule, proof},
          {Ports.facts: Dict.fromArray([(name, rule)]), ruleStyle: None},
        ))
      }
    }
  }
  let make = props => {
    Console.log(props.imports)
    let ruleStyle = props.imports.ruleStyle->Option.getOr(Hybrid)
    let ctx: Context.t = {fixes: [], facts: props.imports.facts}
    let checked = Proof.check(ctx, props.content.proof, props.content.rule)
    <>
      <RuleView rule={props.content.rule} scope={[]} style={ruleStyle}>
        {React.string(props.content.name)}
      </RuleView>
      <ProofView ruleStyle={ruleStyle} scope={[]} proof=checked />
    </>
  }
}
