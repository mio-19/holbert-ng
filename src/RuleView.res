open Signatures
module Make = (
  Term : TERM,
  Judgment : JUDGMENT with module Term := Term,
  JudgmentView : JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment
) => {
  module Rule = Rule.Make(Term, Judgment)

  type style = Gentzen | Linear | Hybrid
  
  type props = {
    rule:Rule.t, 
    style: style, 
    scope?: array<Term.meta>, 
    children: React.element
  }
  module type RULE_COMPONENT = {
    let make : props => React.element
  }
  module Inline : RULE_COMPONENT = {
    let rec make = (props : props) => {
      let {vars, premises,conclusion} = props.rule
      let scope = vars->Array.concat(props.scope->Option.getOr([]))
      let arr = vars->Array.map(JudgmentView.TermView.makeMeta)
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
      let arr = vars->Array.map(JudgmentView.TermView.makeMeta)
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
      let arr = vars->Array.map(JudgmentView.TermView.makeMeta)
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
