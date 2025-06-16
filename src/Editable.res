open ProofEngine
open Signatures

// Describes a React component that is backed by state that has a string
// representation.
// Such components can be passed to WithTextArea or WithTextBox to get a
// component with the same props, but which now can be edited by the user.
module type STRING_BASED = {
  type props
  type state
  let getState : props => state
  let setState : (props,state) => props
  let serialise : (props, state) => string
  let deserialise : (props, string) => result<state,string>
  // this is only required here to "get" the change handler from props.
  // In reality, the onChange function should be given as part of props.
  let onChange : props => result<(),string>
  let make : props => React.element
}

module WithTextArea = (Underlying : STRING_BASED) => {
  type props = Underlying.props
  @react.componentWithProps
  let make = props => {
    let (editing,setEditing) = React.useState (_ => "off")
    let (tree,setTree) = React.useState (_=>Underlying.getState(props))
    let (text,setText) = 
      React.useState (_=> Underlying.serialise(props,tree))
    let done = _ => {
      switch Underlying.deserialise(props,text) {
      | Ok(t) => {
          let foo = Underlying.onChange(Underlying.setState(props,t))
          Console.log(foo)
          switch foo {
          | Ok(()) => {
              setTree(_ => t)
              setText(_=> Underlying.serialise(props,t))
              setEditing(_=>"off")
            }
          | Error(e) => {
              setEditing(_ => e)
            }
          }
        }
      | Error(e) => {
          setEditing(_ => e)
        }
      }
    }
    let onChange= (ev: JsxEvent.Form.t) => {
      let target = JsxEvent.Form.target(ev)
      let value: string = target["value"]
      setText(_ => value)
    }
    if editing == "on" {
      <div>
        <textarea className="editor-textArea" onChange={onChange}>
          {React.string(text)}
        </textarea>
        <div className="editor-controls">
        <span className="editor-button button-icon button-icon-blue typcn typcn-tick" onClick={done}>
        </span>
        <span className="editor-button button-icon button-icon-grey typcn typcn-times" onClick={_ => {
          setEditing(_ => "off")
          setText(_=> Underlying.serialise(props,tree))
        }}>
        </span>
        </div>
      </div>
    } else if editing == "off" {
      <div>{Underlying.make(Underlying.setState(props,tree))}
      <div className="editor-controls">
        <span className="editor-button button-icon button-icon-blue typcn typcn-edit" onClick={_ => setEditing(_ => "on")}>
        </span>
        </div>
      </div>
    } else {
      <div className="editor-error">{React.string(editing)}
        <div className="editor-controls">
        <span className="editor-button button-icon button-icon-blue typcn typcn-edit" 
             onClick={_ => setEditing(_ => "on")}>
        </span>
        <span className="editor-button button-icon button-icon-grey typcn typcn-times" onClick={_ => {
          setText(_=> Underlying.serialise(props,tree))
          setEditing(_ => "off")
        }}>
        </span>
        </div>
      </div>
    }
  }
}

module WithTextBox = (Underlying : STRING_BASED) => {
  type props = Underlying.props
  @react.componentWithProps
  let make = props => {
    
    let (editing,setEditing) = React.useState (_ => "off")
    let (tree,setTree) = React.useState (_=>Underlying.getState(props))
    let (text,setText) = 
      React.useState (_=> Underlying.serialise(props,tree))
    let done = _ => {
      switch Underlying.deserialise(props,text) {
      | Ok(t) => {
          setTree(_ => t)
          switch Underlying.onChange(Underlying.setState(props,t)) {
          | Ok(()) => {
              setTree(_ => t)
              setText(_=> Underlying.serialise(props,t))
              setEditing(_=>"off")
            }
          | Error(e) => {
              setEditing(_ => e)
            }
          }
        }
      | Error(e) => {
          setEditing(_=>e)
        }
      }
    }
    let onChange= (ev: JsxEvent.Form.t) => {
      let target = JsxEvent.Form.target(ev)
      let value: string = target["value"]
      setText(_ => value)
    }
    if editing == "on" {
      <div>
        <input value={text} onChange={onChange}/>
        <div onClick={done}>{React.string("DONE")}</div>
      </div>
    } else if editing == "off" {
      <div onClick={_ => setEditing(_ => "on")}>
        {Underlying.make(Underlying.setState(props,tree))}
      </div>
    } else {
      <div onClick={_ => setEditing(_ => "on")}>{React.string(editing)}</div>
    }
  }
}


module StringSB = {
  type state = string
  type rec props = {
    content: string,
    onChange: props => result<(),string>
  }
  
  let getState = (props) => props.content
  let setState = (props,state) => {...props,content:state}
  let serialise = (props : props,state) => state
  let deserialise = (props,string) => Ok(string)
  let onChange = (props) => props.onChange(props)
  let make = (props) => React.string(props.content)
}

module TermSB = 
  (Term : TERM, TermView : TERM_VIEW with module Term := Term ) => {
  type state = Term.t
  type rec props = {
    term: Term.t, 
    scope: array<Term.meta>, 
    gen?: Term.gen,
    onChange: props => result<(),string>
  }
  let getState = (props) => props.term
  let setState = (props,state) => {...props,term:state}
    
  let serialise = (props : props,state) => {
    state->Term.prettyPrint(~scope=props.scope)
  }
  let onChange = (props) => props.onChange(props)
  let deserialise = (props : props, str : string) => {
    switch Term.parse(str,~scope=props.scope,~gen=?props.gen) {
    | Ok(t,"") => { Ok(t) }
    | Ok(_,_) => Error("additional input beyond expression")
    | Error(e) => Error(e)
    }
  }
  let make = ({term,scope}) => {
    <TermView term=term scope=scope/>
  }
}
module JudgmentSB = (
  Term : TERM, 
  Judgment : JUDGMENT with module Term := Term, 
  JudgmentView : JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment 
  ) => {

  type rec props = {
    judgment: Judgment.t,
    scope: array<Term.meta>,
    gen?: Term.gen, 
    onChange: props => result<(),string>
  }
  type state = Judgment.t
  let getState = (props) => props.judgment 
  let setState = (props,judgment) => {...props,judgment}
  let onChange = (props) => props.onChange(props)
  let serialise = (props : props, state:state) => {
    state->Judgment.prettyPrint(~scope=props.scope)
  }
  let deserialise = (props : props, str : string) => {
    switch Judgment.parse(str,~scope=props.scope,~gen=?props.gen) {
    | Ok(t,"") => Ok(t)
    | Ok(_,_) => Error("additional input beyond expression")
    | Error(e) => Error(e)
    }
  }
  let make = ({judgment,scope}) => {
    <JudgmentView judgment=judgment scope=scope/>
  }
}

module RuleSB =  (
  Term : TERM,
  Judgment : JUDGMENT with module Term := Term,
  JudgmentView : JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment
) => {
  module Rule = Rule.Make(Term,Judgment)
  module RuleView = RuleView.Make(Term,Judgment,JudgmentView)
  
  type rec props = {
    rule: Rule.t, 
    scope: array<Term.meta>,
    name?: string,
    gen?: Term.gen,
    style: RuleView.style,
    onChange: props => ()
  }
  type state = (Rule.t, string)
  let getState = (props) => (props.rule,props.name->Option.getOr(""))
  let setState = (props,(rule,name)) => {...props,rule,name}
  let onChange = (props) => props.onChange(props)
  let serialise = (props : props, state) => {
    state->Rule.prettyPrintTopLevel(~scope=props.scope,~name=?props.name)
  }
  let deserialise = (props : props, str : string) => {
    switch Rule.parseTopLevel(str,~scope=props.scope,~gen=?props.gen) {
    | Ok(r,"") => Ok(r)
    | Ok(_,_) => Error("additional input beyond expression")
    | Error(e) => Error(e)
    }
  }
  let make = (props) => {
    <RuleView rule={props.rule} scope={props.scope} 
       style={props.style}>
       {React.string(props.name->Option.getOr(""))} 
    </RuleView>
  }
}
module TheoremSB = (
  Term : TERM,
  Judgment : JUDGMENT with module Term := Term,
  JudgmentView : JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
  Method : PROOF_METHOD with module Term := Term and module Judgment := Judgment
) => {
  module Rule = Rule.Make(Term, Judgment)
  module RuleView = RuleView.Make(Term,Judgment,JudgmentView)
  module Proof = Proof(Term,Judgment,Method)
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
  let deserialise = (props : props, str : string) => {
    let cur = ref(str)
    Console.log("P1")
    switch Rule.parseTopLevel(cur.contents,~scope=[],~gen=props.gen) {
    | Error(e) => {Console.log("P2"); Error(e)}
    | Ok(((r,n),s)) => switch Proof.parse(s,~scope=[],~gen=props.gen) {
      | Error(e) => Error(e)
      | Ok((_,s')) if String.length(String.trim(s')) > 0 =>
          Error("Trailing input")
      | Ok((prf,_)) => {
          let ctx : Context.t = {fixes:[],facts:props.facts}
          Ok({name:n,rule:r,proof:Proof.check(ctx,prf,r)})
        }
      }
    }
  }
  let make = (props) => {
    <RuleView rule={props.rule} scope={[]} 
       style={props.style}>
       {React.string(props.name)} 
    </RuleView>
  }
}
module RuleSetSB = (
  Term : TERM,
  Judgment : JUDGMENT with module Term := Term,
  JudgmentView : JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment
) => {
  module Rule = Rule.Make(Term, Judgment)
  module RuleView = RuleView.Make(Term,Judgment,JudgmentView)
  module Name = WithTextBox(StringSB)
  type state = Dict.t<Rule.t>
  type rec props = {
    rules: Dict.t<Rule.t>,
    style: RuleView.style,
    onChange: props => result<(),string>
  }
  let getState = (props) => props.rules
  let setState = (props,state) => {...props,rules:state}
  let onChange = (props) => props.onChange(props)
  let serialise = (props : props, state) => {
    let results = [];
    state->Dict.forEachWithKey((value, key) => {
      results->Array.push(value->Rule.prettyPrintTopLevel(~scope=[],~name=key))
    })
    results->Array.join(Util.newline)
  }
  let deserialise = (props : props, str : string) => {
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
  let make = (props) => {
    <div className={"axiom-set axiom-set-"->String.concat(String.make(props.style))}>
    { Dict.toArray(props.rules)->Array.map(((n,r)) => 
      <RuleView rule={r} scope={[]} style={props.style}>
        <Name content={n} onChange={(_) => Error("BLAH!")} />
      </RuleView>)
    ->React.array
    }
    </div>
  }
}
