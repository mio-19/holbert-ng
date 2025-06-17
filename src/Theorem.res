open Signatures
open Component
open Method
module Make = (
  Term : TERM, 
  Judgment : JUDGMENT with module Term := Term,
  JudgmentView : JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  Method : PROOF_METHOD with module Term := Term and module Judgment := Judgment
) => {
  module Rule = Rule.Make(Term,Judgment)
  module Proof = Proof.Make(Term,Judgment,Method)
  module Context = Context(Term,Judgment)
  
  open RuleView
  module RuleView = RuleView.Make(Term,Judgment,JudgmentView)
  module Ports = Ports(Term,Judgment)
  type props = { content: string, imports: Ports.t, onLoad: (~exports:Ports.t) => (), onChange: (string,~exports:Ports.t) => () }
  type state = {name: string, rule: Rule.t, proof: Proof.checked}
  let deserialise = (facts : Dict.t<Rule.t>, gen : Term.gen, str : string) : result<state,string> => {
    let cur = ref(str)
    switch Rule.parseTopLevel(cur.contents,~scope=[],~gen=gen) {
    | Error(e) => Error(e)
    | Ok(((r,n),s)) => switch Proof.parse(s,~scope=[],~gen=gen) {
      | Error(e) => Error(e)
      | Ok((_,s')) if String.length(String.trim(s')) > 0 =>
          Error("Trailing input: "->String.concat(s'))
      | Ok((prf,_)) => {
          let ctx : Context.t = {fixes:[],facts}
          Ok({name:n,rule:r,proof:Proof.check(ctx,prf,r)})
        }
      }
    }
  }
  let make = (props) => {
    
    let gen = Term.makeGen()
    switch deserialise(props.imports.facts, gen, props.content) {
    | Ok(state) => {
        props.onLoad(~exports={facts: Dict.fromArray([(state.name,state.rule)]), ruleStyle: None})
        <RuleView rule={state.rule} scope={[]} 
          style={props.imports.ruleStyle->Option.getOr(Hybrid)}>
          {React.string(state.name)} 
        </RuleView>
      }
    | Error(err) => <div className="error"> {React.string(err)} </div>
    }
  }
}
