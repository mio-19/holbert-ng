open Demo
module type TERM_VIEW = {
  module Term : TERM
  type props = {term: Term.t, scope: array<Term.meta>}
  let make : props => React.element
  let makeMeta : Term.meta => React.element
}

module type JUDGMENT_VIEW = {
  module Base : BASE 
  open Base
  module TermView : TERM_VIEW with module Term := Term;
  type props = {judgment: Judgment.t, scope: array<Term.meta>}
  let make : props => React.element
} 

module SExpView : TERM_VIEW with module Term := SExp = {

  type props = {term: SExp.t, scope: array<string>}
  let viewVar = (idx, scope:array<string>) => {
    switch scope[idx] {
    | Some(n) if Array.indexOf(scope,n) == idx => <span className="term-metavar"> { React.string(n) } </span>
    | _ => <span className="term-metavar-unnamed"> {React.string("\\")} { React.int(idx) } </span>
    }
  }
  let makeMeta = (str : string) => {
    <span className="rule-binder">{React.string(str)}{React.string(".")}</span>
  }
  let parenthesise = (f) =>
    [<span className="symbol">{React.string("(")}</span>,...f,<span className="symbol">{React.string(")")}</span>]
  let intersperse = (a) => a->Array.flatMapWithIndex((e, i) => 
    if i == 0 { [e] } else { [React.string(" "),e] })
  @react.componentWithProps
  let rec make = ({term, scope}) => switch term {
  | Compound({subexps:bits}) => {
      <span className="term-compound">
      {bits
        ->Array.map(t => make({term:t,scope}))
        ->intersperse->parenthesise->React.array} 
      </span>
      }
  | Var({idx:idx}) => viewVar(idx,scope)
  | Symbol({name:s}) => <span className="term-const"> { React.string(s) } </span>
  | Schematic({schematic:s, allowed:vs}) => 
      <span className="term-schematic">
        {React.string("?")} {React.int(s)} 
        <span className="term-schematic-telescope">
          {vs
            ->Array.map(v => viewVar(v,scope))
            ->intersperse->parenthesise->React.array}
        </span>
      </span>
  }
}
module SExpBase = {module Term = SExp; module Judgment = SExp}
module SExpJView : JUDGMENT_VIEW with module Base := SExpBase = {
  module TermView = SExpView
  type props =  {judgment: SExp.t, scope: array<string>}
  let make = (props : props) => SExpView.make({term: props.judgment,scope:props.scope})
}

module RuleView = (Base : BASE, 
  TermView : TERM_VIEW with module Term := Base.Term,
  JudgmentView : JUDGMENT_VIEW with module Base := Base) => {
  module PE = ProofEngine(Base)
  open PE
  type style = Gentzen | Linear | Hybrid
  
  type props = {rule:Rule.t, style: style, scope?: array<Base.Term.meta>, children: React.element}
  module type RULE_COMPONENT = {
    let make : props => React.element
  }
  module Inline : RULE_COMPONENT = {
    let rec make = (props : props) => {
      let {vars, premises,conclusion} = props.rule
      let scope = vars->Array.concat(props.scope->Option.getOr([]))
      let arr = vars->Array.map(TermView.makeMeta)
      Array.reverse(arr)
      <span className="inline-rule">
        <span className="rule-rulename-defined">
        {props.children}
        </span>
        {if Array.length(arr) > 0 { <span className="rule-binders">{React.array(arr)}</span> }
          else { React.string("") }}
        {React.array(premises
          ->Array.map(p=><span className="rule-context">
              {make({rule:p, scope,children:React.string(""),style:props.style})}</span>)
          ->Array.flatMapWithIndex( (e, i) => 
              if i == 0 {
                [e]
              } else {
                [<span className="symbol symbol-bold symbol-comma">{React.string(",")}</span>,e]
              }))}
        {if premises->Array.length > 0 { 
          <span className="symbol symbol-turnstile symbol-bold">
            {React.string("⊢")}
          </span> 
        } else { 
          React.string("")
        }}
        <JudgmentView judgment={conclusion} 
          scope={scope} />
      </span>
    }
  }
  module Hypothetical = (Premise : RULE_COMPONENT) => {
    let make = (props : props) => if Array.length(props.rule.premises) == 0 { Inline.make(props) } else {
      let {vars, premises,conclusion} = props.rule
      let arr = vars->Array.map(TermView.makeMeta)
      Array.reverse(arr)
      let scope = vars->Array.concat(props.scope->Option.getOr([]))
      <table className="inference">
      <tr><td className="rule-cell rule-binderbox" rowSpan=3>{React.array(arr)}</td>
        {React.array(premises->Array.map(p=>
          <td className="rule-cell rule-premise">
            <Premise rule={p} scope={scope} style={props.style}>
              {React.string("")}
            </Premise>
          </td>))}
        <td className="rule-cell rule-spacer"></td>
        <td rowSpan=3 className="rule-cell rule-rulebox">
          <span className="rule-rulename-defined">
            {props.children}
          </span>
        </td>
      </tr>
      <tr> <td colSpan={premises->Array.length + 1} className="rule-cell">
        {React.string("⋮")}
      </td></tr>
      <tr>
        <td colSpan={premises->Array.length + 1} className="rule-cell rule-hypothetical-conclusion">
          <JudgmentView judgment={conclusion} scope={scope} />
        </td>
      </tr></table>
    }
  }
  module TopLevel = (Premise : RULE_COMPONENT) => {
    let make = (props : props) => {
      let {vars, premises,conclusion} = props.rule
      let arr = vars->Array.map(TermView.makeMeta)
      Array.reverse(arr)
      let scope = vars->Array.concat(props.scope->Option.getOr([]))
      <div className="axiom"><table className="inference">
      <tr><td className="rule-cell rule-binderbox" rowSpan=2>{React.array(arr)}</td>
        {React.array(premises->Array.map(p=>
          <td className="rule-cell rule-premise">
            <Premise rule={p} scope={scope} style={props.style}>
              {React.string("")}
            </Premise>
          </td>))}
        <td className="rule-cell rule-spacer"></td>
        <td rowSpan=2 className="rule-cell rule-rulebox">
          <span className="rule-rulename-defined">
            {props.children}
          </span>
        </td>
      </tr>
      <tr>
        <td colSpan={premises->Array.length + 1} className="rule-cell rule-conclusion">
          <JudgmentView judgment={conclusion} scope={scope} />
        </td>
      </tr></table></div>
    }
  }
  module Gentzen = TopLevel(Hypothetical(Inline))
  module Hybrid = TopLevel(Inline)
  
  @react.componentWithProps
  let make = (props) => {
    switch (props.style) {
    | Hybrid => Hybrid.make(props)
    | Gentzen => Gentzen.make(props)
    | Linear => Inline.make(props)
    }
  }
} 



module type TEXTUAL_COMPONENT = {
  type props
  type state
  let getState : props => state
  let setState : (props,state) => props
  let serialise : (props, state) => string
  let deserialise : (props, string) => result<state,string>
  let onChange : props => ()
  let make : props => React.element
}

module TermTextualComponent = 
  (Term : TERM, TermView : TERM_VIEW with module Term := Term ) => {
  type state = Term.t
  type rec props = {
    term: Term.t, 
    scope: array<Term.meta>, 
    gen?: Term.gen,
    onChange: props => ()
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
module JudgmentTextualComponent =
  (Base : BASE, JudgmentView : JUDGMENT_VIEW with module Base := Base ) => {
  open Base
  type rec props = {
    judgment: Judgment.t,
    scope: array<Term.meta>,
    gen?: Term.gen, 
    onChange: props => ()
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
module RuleTextualComponent =  (
  Base : BASE,
  TermView : TERM_VIEW with module Term := Base.Term,
  JudgmentView : JUDGMENT_VIEW with module Base := Base
) => {
  module PE = ProofEngine(Base)
  open PE
  open Base
  module TheRuleView = RuleView(Base,TermView,JudgmentView)
  type rec props = {
    rule: Rule.t, 
    scope: array<Term.meta>,
    name?: string,
    gen?: Term.gen,
    style: TheRuleView.style,
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
    <TheRuleView rule={props.rule} scope={props.scope} 
       style={props.style}>
       {React.string(props.name->Option.getOr(""))} 
    </TheRuleView>
  }
}

module RuleSetTextualComponent =  (
  Base : BASE,
  TermView : TERM_VIEW with module Term := Base.Term,
  JudgmentView : JUDGMENT_VIEW with module Base := Base
) => {
  module PE = ProofEngine(Base)
  open PE
  open Base
  module TheRuleView = RuleView(Base,TermView,JudgmentView)
  type state = Dict.t<Rule.t>
  type rec props = {
    rules: Dict.t<Rule.t>,
    style: TheRuleView.style,
    onChange: props => ()
  }
  let getState = (props) => props.rules
  let setState = (props,state) => {...props,rules:state}
  let onChange = (props) => props.onChange(props)
  let serialise = (props : props, state) => {
    let results = [];
    state->Dict.forEachWithKey((value, key) => {
      results->Array.push(value->Rule.prettyPrintTopLevel(~scope=[],~name=key))
    })
    results->Array.join(Demo.newline)
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
      <TheRuleView rule={r} scope={[]} style={props.style}>
        {React.string(n)} 
      </TheRuleView>)
    ->React.array
    }
    </div>
  }
}

module Editable = (Underlying : TEXTUAL_COMPONENT) => {
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
          Underlying.onChange(Underlying.setState(props,t))
          setText(_=> Underlying.serialise(props,t))
          setEditing(_=>"off")
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
      <div><input value={text} onChange={onChange}/><div onClick={done}>{React.string("DONE")}</div></div>
    } else if editing == "off" {
      <div onClick={_ => setEditing(_ => "on")}>
        {Underlying.make(Underlying.setState(props,tree))}
      </div>
    } else {
      <div onClick={_ => setEditing(_ => "on")}>{React.string(editing)}</div>
    }
  }
}

module EditableTextArea = (Underlying : TEXTUAL_COMPONENT) => {
  type props = Underlying.props
  @react.componentWithProps
  let make = props => {
    Console.log(("MK",props))
    let (editing,setEditing) = React.useState (_ => "off")
    let (tree,setTree) = React.useState (_=>Underlying.getState(props))
    let (text,setText) = 
      React.useState (_=> Underlying.serialise(props,tree))
    let done = _ => {
      switch Underlying.deserialise(props,text) {
      | Ok(t) => {
          setTree(_ => t)
          Underlying.onChange(Underlying.setState(props,t))
          setText(_=> Underlying.serialise(props,t))
          setEditing(_=>"off")
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

module PE2 = ProofEngine(SExpBase)
module RuleSExpTE = RuleSetTextualComponent(SExpBase,SExpView,SExpJView)
module RuleSExpView = EditableTextArea(RuleSExpTE)
include PE2.Rule
include RuleSExpView//(RuleSExpView.Hypothetical(RuleSExpView.Inline))
//include SExpBaseView

