
open Signatures

module Make = (Term : TERM, Judgment : JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term,Judgment)
  module Step = {
    type t<'a> = {
      fixes: array<Term.meta>, 
      facts: Dict.t<Rule.t>,
      proof: 'a
    }
    let bind : (t<'a>, 'a => t<'b>) => t<'b> = (s, f) => {
      let t = f(s.proof);
      { 
        fixes: t.fixes->Array.concat(s.fixes),
        facts: Dict.copy(s.facts)->Dict.assign(t.facts),
        proof: t.proof
      }
    }
  }
  module Goal = {
    type t = {
      fix: array<Term.meta>, 
      assume: array<Rule.t>, 
      assumeNames: array<string>, 
      show: Judgment.t 
    }
    let toRule : t => Rule.t = (goal : t) => {
      vars: goal.fix,
      premises: goal.assume,
      conclusion: goal.show
    }
  }
  module type PROOFT = {
    type t<'a>
    let subproofs : (Goal.t,t<'a>, 'a=>Goal.t) => Dict.t<Step.t<'a>>
  }
  module Either = (A : PROOFT, B : PROOFT) => {
    type t<'a> = L(A.t<'a>) | R(B.t<'a>)
    let subproofs = (goal:Goal.t,it : t<'a>, f : 'a => Goal.t) => switch it {
    | L(x) => A.subproofs(goal,x,f)
    | R(x) => B.subproofs(goal,x,f)
    }
  }
  
  module Deduction : PROOFT = {
    type t<'a> = {
      from: array<'a>,
      ruleName: string,
      instantiation: array<Term.t>
    }
    let subproofs = (goal : Goal.t,it : t<'a>, _ : 'a => Goal.t) => {
      let toStep : 'a => Step.t<'a> = (a) => {
        fixes: goal.fix, 
        facts: Belt.Array.zip(goal.assumeNames,goal.assume)->Dict.fromArray,
        proof: a
      };
      Belt.Array.range(0,Array.length(it.from))
        ->Array.map(x =>Belt.Int.toString(x))
        ->Belt.Array.zip(it.from->Array.map(toStep))
        ->Dict.fromArray
    }
  }
  module Lemma : PROOFT = {
    type t<'a> = {
      name: string,
      have: 'a,
      show: 'a
    }
    let subproofs = (goal: Goal.t,it : t<'a>, f : 'a => Goal.t) => {
      let haveGoal : Step.t<'a> = {
        fixes: goal.fix, 
        facts: Belt.Array.zip(goal.assumeNames,goal.assume)->Dict.fromArray,
        proof: it.have
      };
      let showGoal : Step.t<'a> = {
        fixes: goal.fix, 
        facts: Belt.Array.zip(goal.assumeNames,goal.assume)
          ->Array.concat([(it.name,f(it.have)->Goal.toRule)])
          ->Dict.fromArray,
        proof: it.show
      };
      Dict.fromArray([(it.name,haveGoal), ("@show",showGoal)])
    }
  }
  module Proof = (A : PROOFT) => {
    type rec t = {goal:Goal.t, step?: A.t<t> }
  }
}
//module TestProofs = Proof(Either(Deduction,Lemma))