type props = {term: SExp.t, scope: array<string>}

let viewVar = (idx, scope:array<string>) => switch scope[idx] {
| Some(n) if Array.indexOf(scope,n) == idx => 
  <span className="term-metavar">
    { React.string(n)}
  </span>
| _ => 
  <span className="term-metavar-unnamed">
    {React.string("\\")} { React.int(idx) }
  </span>
}

let makeMeta = (str : string) =>
  <span className="rule-binder">
    {React.string(str)}{React.string(".")}
  </span>

let parenthesise = (f) => [
  <span className="symbol">
    {React.string("(")}
  </span>,
  ...f,
  <span className="symbol">
    {React.string(")")}
  </span>
]
    
let intersperse = (a) => 
  a->Array.flatMapWithIndex((e, i) => if i == 0 { [e] } else { [React.string(" "),e] })

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
