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
  type props = {
    content: string,
    imports: Ports.t,
    onLoad: (~exports: Ports.t, ~string: string=?) => unit,
    onChange: (string, ~exports: Ports.t) => unit,
  }
  type state = {name: string, rule: Rule.t, proof: Proof.checked}
  let deserialise = (facts: Dict.t<Rule.t>, gen: Term.gen, str: string): result<state, string> => {
    let cur = ref(str)
    switch Rule.parseTopLevel(cur.contents, ~scope=[], ~gen) {
    | Error(e) => Error(e)
    | Ok(((r, n), s)) =>
      switch Proof.parse(s, ~scope=[], ~gen) {
      | Error(e) => Error(e)
      | Ok((_, s')) if String.length(String.trim(s')) > 0 =>
        Error("Trailing input: "->String.concat(s'))
      | Ok((prf, _)) => {
          let ctx: Context.t = {fixes: [], facts}
          Ok({name: n, rule: r, proof: Proof.check(ctx, prf, r)})
        }
      }
    }
  }
  let make = props => {
    let gen = Term.makeGen()
    switch deserialise(props.imports.facts, gen, props.content) {
    | Ok(state) => {
        React.useEffect(() => {
          let export = Dict.fromArray([(state.name, state.rule)])
          props.onLoad(~exports={Ports.facts: export, ruleStyle: None})
          None
        }, [])
        let ruleStyle = props.imports.ruleStyle->Option.getOr(Hybrid)
        <>
          <RuleView rule={state.rule} scope={[]} style={ruleStyle}>
            {React.string(state.name)}
          </RuleView>
          <ProofView ruleStyle={ruleStyle} scope={[]} proof=state.proof />
        </>
      }
    | Error(err) => {
        React.useEffect(() => {
          props.onLoad(~exports={Ports.facts: Dict.make(), ruleStyle: None})
          None
        }, [])
        <div className="error"> {React.string(err)} </div>
      }
    }
  }
}
