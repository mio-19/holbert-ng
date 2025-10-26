open Util
type idx_props = {idx: int, scope: array<string>}
let viewVar = (props: idx_props) =>
  switch props.scope[props.idx] {
  | Some(n) if Array.indexOf(props.scope, n) == props.idx =>
    <span className="term-metavar"> {React.string(n)} </span>
  | _ =>
    <span className="term-metavar-unnamed">
      {React.string("\\")}
      {React.int(props.idx)}
    </span>
  }

let makeMeta = (str: string) =>
  <span className="rule-binder">
    {React.string(str)}
    {React.string(".")}
  </span>

let parenthesise = f =>
  [
    <span className="symbol" key={"-1"}> {React.string("(")} </span>,
    ...f,
    <span className="symbol" key={"-2"}> {React.string(")")} </span>,
  ]

let intersperse = a =>
  a->Array.flatMapWithIndex((e, i) =>
    if i == 0 {
      [e]
    } else {
      [React.string(" "), e]
    }
  )
type props1 = {term: HOTerm.t, scope: array<string>, brackets: bool}
@react.componentWithProps
let rec make1 = ({term, scope, brackets}) =>
  switch term {
  | Var({idx}) => viewVar({idx, scope})
  | Symbol({name: s}) => <span className="term-const"> {React.string(s)} </span>
  | Schematic({schematic: s}) =>
    <span className="term-schematic">
      {React.string("?")}
      {React.int(s)}
    </span>
  | App(_) =>
    switch HOTerm.strip(term) {
    | (Symbol({name: "="}), args) if Array.length(args) == 2 =>
      <span className="term-equality">
        {React.createElement(make1, {term: args->Array.getUnsafe(0), scope, brackets: true})}
        {React.string("=")}
        {React.createElement(make1, {term: args->Array.getUnsafe(1), scope, brackets: true})}
      </span>
    | (func, args) =>
      let xs = Array.concat([func], args)
      let a =
        <span className="term-app">
          {xs
          ->Array.mapWithIndex((t, i) =>
            React.createElement(make1, withKey({term: t, scope, brackets: true}, i))
          )
          ->intersperse
          ->React.array}
        </span>
      if brackets {
        [a]->parenthesise->React.array
      } else {
        a
      }
    }
  | Lam({name, body}) => {
      let newScope = Array.concat([name], scope)
      <span className="term-lambda">
        {React.string(name)}
        {React.createElement(make1, {term: body, scope: newScope, brackets: false})}
      </span>
    }
  | Unallowed => <p> {React.string("Internal error: unallowed")} </p>
  }
type props = {term: HOTerm.t, scope: array<string>}
@react.componentWithProps
let make = ({term, scope}) => make1({term, scope, brackets: false})
