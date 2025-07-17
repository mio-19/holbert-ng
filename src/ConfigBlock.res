open Signatures
open Component

module Make = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module Ports = Ports(Term, Judgment)
  open RuleView
  type props = {
    content: string,
    imports: Ports.t,
    onLoad: (~exports: Ports.t) => unit,
    onChange: (string, ~exports: Ports.t) => unit,
  }
  let deserialise = str =>
    switch str {
    | "Gentzen" => Gentzen
    | "Linear" => Linear
    | "Hybrid" => Hybrid
    | _ => Hybrid
    }
  let make = props => {
    let (style, setStyle) = React.useState(_ => deserialise(props.content))
    React.useEffect(() => {
      props.onLoad(~exports={Ports.facts: Dict.make(), ruleStyle: Some(style)})
      None
    }, [])
    let onChange = e => {
      let target = JsxEvent.Form.target(e)
      let value: string = target["value"]
      let sty = deserialise(value)
      setStyle(_ => sty)

      props.onChange(value, ~exports={Ports.facts: Dict.make(), ruleStyle: Some(sty)})
    }
    [Gentzen, Linear, Hybrid]
    ->Array.map(n =>
      <input
        type_="radio"
        id={"style_"->String.concat(String.make(n))}
        key={String.make(n)}
        name="style"
        onChange
        value={String.make(n)}
        checked={style == n}
      />
    )
    ->React.array
  }
}
