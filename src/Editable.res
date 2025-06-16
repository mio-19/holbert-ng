open Method
open Signatures

module type PORTS = {
  type t
  let combine : (t,t) => t
  let empty: t
}
module Ports = (Term : TERM, Judgment : JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term, Judgment)
  type t = { facts: Dict.t<Rule.t>, ruleStyle: option<RuleView.style>  }
  let empty = { facts: Dict.make(), ruleStyle: None }
  let combine = (p1,p2) => {
    let facts = Dict.copy(p1.facts)->Dict.assign(p2.facts);
    let ruleStyle = p2.ruleStyle->Option.mapOr(p1.ruleStyle, x => Some(x));
    { facts, ruleStyle }
  }
}


module type COMPONENT = {
  module Ports : PORTS
  type props = { content: string, imports: Ports.t, onChange: (string,~exports:Ports.t) => () }
  let make :  props => React.element
  let 
}

module Config = (
  Term : TERM, 
  Judgment : JUDGMENT with module Term := Term
) => {
  module Ports = Ports(Term,Judgment)
  open RuleView
  type props = { content: string, imports: Ports.t, onLoad: (~exports:Ports.t) => (), onChange: (string,~exports:Ports.t) => () }
  let deserialise = (str) => switch str {
  | "Gentzen" => Gentzen
  | "Linear" => Linear
  | "Hybrid" => Hybrid
  | _ => Hybrid
  }
  let make = (props) => {
    let (style,setStyle) = React.useState (_=>deserialise(props.content))
    React.useEffect(() => {
      props.onLoad(~exports={Ports.facts: Dict.make(), ruleStyle:Some(style)})  
      None
    },[])
    let onChange= (e) => {
      let target = JsxEvent.Form.target(e)
      let value: string = target["value"]
      setStyle(_ => deserialise(value))
      props.onChange(value,~exports={Ports.facts: Dict.make(), ruleStyle:Some(style)})
    }
    [Gentzen,Linear,Hybrid]->Array.map(n =>
      <input type_="radio" id={"style_"->String.concat(String.make(n))} name="style" onChange value={String.make(n)} checked={style==n}/>
    )->React.array
    
  }
}


module TextAreaEditor = (Underlying : COMPONENT) => {
  module Ports = Underlying.Ports
  type props = { content: string, imports: Ports.t, onLoad: (~exports:Ports.t) => (), onChange: (string,~exports:Ports.t) => () }
  let make = (props) => {
    let (editing,setEditing) = React.useState (_ => false)
    let (text,setText) = React.useState (_ => props.content)
    let onTextChange= (ev: JsxEvent.Form.t) => {
      let target = JsxEvent.Form.target(ev)
      let value: string = target["value"]
      setText(_ => value)
    }
    let done = (_) => {
      setEditing(_=>false)
    }
    if editing {
      <div>
        <textarea className="editor-textArea" onChange={onTextChange}>
          {React.string(text)}
        </textarea>
        <div className="editor-controls">
        <span className="editor-button button-icon button-icon-blue typcn typcn-tick" onClick={done}>
        </span>
        </div>
      </div>
    } else {
      <div>
      {Underlying.make({
        content:text, 
        imports: props.imports, 
        onLoad: props.onLoad,
        onChange: (string, ~exports) => {
          setText(_=>string)
          props.onChange(string,~exports)
        }
      })}
      <div className="editor-controls">
      <span className="editor-button button-icon button-icon-blue typcn typcn-edit" onClick={_ => setEditing(_ => true)}>
      </span>
      </div>
      </div>
    }
  }
}

module AxiomSet = (
  Term : TERM, 
  Judgment : JUDGMENT with module Term := Term,
  JudgmentView : JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment) => {
  module Rule = Rule.Make(Term,Judgment)
  module RuleView = RuleView.Make(Term,Judgment,JudgmentView)
  module Ports = Ports(Term,Judgment)
  type props = { content: string, imports: Ports.t, onLoad: (~exports:Ports.t) => (), onChange: (string,~exports:Ports.t) => () }
  
  let deserialise = (str: string) => {
    let cur = ref(str)
    let go = ref(true)
    let results = Dict.make()    
    let ret = ref(Error("impossible"))
    while go.contents {
      switch Rule.parseTopLevel(cur.contents,~scope=[]) {
      | Ok((t,n),rest) => {
          if n->String.trim == "" {
            go := false
            ret := Error("Rule given with no name")
          } else { 
            Dict.set(results,n,t)
            if rest->String.trim == "" {
              go := false
              ret := Ok(results)
            } else {
              cur := rest
            }
          }
        }
      | Error(e) => { go := false; ret := Error(e) }
      }
    }
    ret.contents
  }
  
  let make = (props ) => {
    switch deserialise(props.content) {
    | Ok(s) => {
        props.onLoad(~exports= {Ports.facts: s, ruleStyle: None});
        <div className={"axiom-set axiom-set-"->String.concat(String.make(props.imports.ruleStyle->Option.getOr(Hybrid)))}>
        { Dict.toArray(s)->Array.map(((n,r)) => 
          <RuleView rule={r} scope={[]} style={props.imports.ruleStyle->Option.getOr(Hybrid)}>
            {React.string(n)}
            //<Name content={n} onChange={(_) => Error("BLAH!")} />
          </RuleView>)
          ->React.array
        }
        </div>
      }
    | Error(e) => <div className="error"> {React.string(e)} </div>
    }
  }
}
module Theorem = (
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

module TheoremSB = (
  Term : TERM,
  Judgment : JUDGMENT with module Term := Term,
  JudgmentView : JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  Method : PROOF_METHOD with module Term := Term and module Judgment := Judgment
) => {
  module Rule = Rule.Make(Term, Judgment)

  module Proof = Proof.Make(Term,Judgment,Method)
  module Context = Context(Term,Judgment)
  type state = { name : string, rule: Rule.t, proof: Proof.checked }
  type rec props = {
    name: string,
    rule: Rule.t,
    proof: Proof.checked,
    facts: Dict.t<Rule.t>,
    gen : Term.gen,
    style : RuleView.style,
    onChange: props => result<(),string>
  }
  module RuleView = RuleView.Make(Term,Judgment,JudgmentView)
  let getState : props => state 
    = ({name,rule,proof,gen:_,onChange:_,style:_,facts:_}) 
      => {name,rule,proof}
  let setState : (props,state) => props = (props,{name,rule,proof}) 
    => {name,rule,proof, style:props.style,
        onChange:props.onChange,gen: props.gen,facts: props.facts}
  let onChange = (props) => props.onChange(props)
  let serialise = (props : props, state : state) => {
    state.rule
      ->Rule.prettyPrintTopLevel(~scope=[],~name=state.name)
      ->String.concat(newline)
      ->String.concat(Proof.prettyPrint(Proof.uncheck(state.proof), ~scope=[]))
  }
  
}