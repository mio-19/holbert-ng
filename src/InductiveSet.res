open Signatures
open Component

// InductiveSet is specific to HOTerm to allow pattern matching on term structure
module Make = (
  Term: TERM with type t = HOTerm.t,
  Judgment: JUDGMENT with module Term := Term and type t = HOTerm.t,
  JudgmentView: JUDGMENT_VIEW with module Term := Term and module Judgment := Judgment,
) => {
  module Rule = Rule.Make(Term, Judgment)
  module RuleView = RuleView.Make(Term, Judgment, JudgmentView)
  module Ports = Ports(Term, Judgment)
  type state = dict<Rule.t>
  type props = {
    content: state,
    imports: Ports.t,
    onChange: (state, ~exports: Ports.t=?) => unit,
  }

  let isIntroduction = (rule: Rule.t): bool => {
    let (head, _args) = HOTerm.strip(rule.conclusion)

    switch head {
    | Symbol(_) => true
    | _ => false
    }
  }

  let introRoot = (rule: Rule.t): option<(string, int)> => {
    let (head, args) = HOTerm.strip(rule.conclusion)

    switch head {
    | Symbol({name}) => Some((name, Array.length(args)))
    | _ => None
    }
  }

  type constructorKey = (string, int)
  let groupByConstructor = (rules: dict<Rule.t>): dict<array<(string, Rule.t)>> => {
    // filter to only introduction rules and extract their roots
    let introRules =
      Dict.toArray(rules)
      ->Array.map(((name, rule)) => (name, rule, introRoot(rule)))
      ->Array.filter(((_, _, root)) => root->Option.isSome)
      ->Array.map(((name, rule, root)) => (name, rule, root->Option.getUnsafe))

    let grouped = Dict.make()
    Array.forEach(introRules, ((name, rule, (constructorName, arity))) => {
      let key = constructorName ++ "§" ++ Int.toString(arity)
      switch Dict.get(grouped, key) {
      | None => Dict.set(grouped, key, [(name, rule)])
      | Some(existing) => Dict.set(grouped, key, Array.concat(existing, [(name, rule)]))
      }
    })

    grouped
  }

  let derived = (state: state): state => {
    let groups = groupByConstructor(state)

    Console.log("=== Inductive Set Grouping ===")
    Dict.toArray(groups)->Array.forEach(((key, rules)) => {
      Console.log2("Group", key)
      Array.forEach(rules, ((name, rule)) => {
        let root = introRoot(rule)
        Console.log2(name, root)
      })
    })
    Console.log("=== End Grouping ===")

    // For now, just return empty - we'll generate induction rules here later
    Dict.make()
  }
  let serialise = (state: state) => {
    state
    ->Dict.toArray
    ->Array.map(((k, r)) => r->Rule.prettyPrintTopLevel(~name=k))
    ->Array.join("\n")
  }
  let deserialise = (str: string, ~imports as _: Ports.t) => {
    let cur = ref(str)
    let go = ref(true)
    let results = Dict.make()
    let ret = ref(Error("impossible"))
    while go.contents {
      switch Rule.parseTopLevel(cur.contents, ~scope=[]) {
      | Ok((t, n), rest) =>
        if n->String.trim == "" {
          go := false
          ret := Error("Rule given with no name")
        } else {
          Dict.set(results, n, t)
          if rest->String.trim == "" {
            go := false
            ret := Ok(results)
          } else {
            cur := rest
          }
        }
      | Error(e) => {
          go := false
          ret := Error(e)
        }
      }
    }
    ret.contents->Result.map(state => (state, {Ports.facts: state, ruleStyle: None}))
  }

  let make = props => {
    <div
      className={"axiom-set axiom-set-"->String.concat(
        String.make(props.imports.ruleStyle->Option.getOr(Hybrid)),
      )}>
      {Dict.toArray(props.content->Dict.copy->Dict.assign(derived(props.content)))
      ->Array.mapWithIndex(((n, r), i) =>
        <RuleView
          rule={r}
          scope={[]}
          key={String.make(i)}
          style={props.imports.ruleStyle->Option.getOr(Hybrid)}>
          {React.string(n)}
        </RuleView>
      )
      ->React.array}
    </div>
  }
}
