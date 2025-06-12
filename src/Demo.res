
let mapMapValues = (m: Map.t<'a,'b>, f: 'b => 'c) => {
  let nu = Map.make();
  m->Map.forEachWithKey((v,k) => {
    nu->Map.set(k,f(v))
  })
  nu
}


module type TERM = {
  type t
  type schematic
  type meta
  type subst = Map.t<schematic,t>
  let substitute : (t, subst) => t
  let unify : (t, t) => array<subst>
  let substDeBruijn  : (t, array<t>, ~from:int=?) => t
  let upshift : (t, int, ~from:int=?) => t    
  type gen
  let fresh : (gen, ~replacing:meta=?) => schematic
  let seen : (gen, schematic) => ()
  let place : (schematic, ~scope: array<meta>) => t
  let makeGen : () => gen
  let parse : (string, ~scope: array<meta>, ~gen: gen=?) => result<(t,string),string>
  let parseMeta : (string) => result<(meta,string),string>
  let prettyPrint : (t, ~scope: array<meta>) => string
  let prettyPrintMeta : (meta) => string
}

module type BASE = {
  module Term : TERM
  module Judgment : {
    type t
    let substitute : (t, Term.subst) => t
    let unify : (t, t) => array<Term.subst>
    let substDeBruijn  : (t, array<Term.t>, ~from:int=?) => t
    let upshift : (t, int, ~from:int=?) => t
    let parse : (string, ~scope: array<Term.meta>, ~gen: Term.gen=?) => result<(t,string),string>
    let prettyPrint : (t, ~scope: array<Term.meta>) => string
  }
}
let newline = "\n"
let vinculumRES = "^\s*\\n\\s*[-—][-—][\\-—]+([^\\n\\-—][^\\n]*)?"
module ProofEngine = (Base : BASE) => {
  open Base
  module Rule = {
    type rec t = {vars: array<Term.meta>, premises: array<t>, conclusion: Judgment.t}
    let rec substitute = (rule: t, subst: Term.subst) => {
      let subst' = subst->mapMapValues(v => v->Term.upshift(Array.length(rule.vars)));
      { 
        vars: rule.vars, 
        premises: rule.premises->Array.map(premise => premise->substitute(subst')),
        conclusion: rule.conclusion->Judgment.substitute(subst')
      }
    }
    let rec substDeBruijn = (rule: t, substs: array<Term.t>, ~from:int=0) => {
      let len = Array.length(rule.vars)
      let substs' = substs->Array.map(v => v->Term.upshift(len,~from=from))
      {
        vars: rule.vars,
        premises: rule.premises
          ->Array.map(premise => premise->substDeBruijn(substs', ~from=from+len)),
        conclusion: rule.conclusion
          ->Judgment.substDeBruijn(substs',~from=from+len),
      }
    }
    let rec upshift = (rule: t, amount: int, ~from:int=0) => {
      let len = Array.length(rule.vars)
      {
        vars: rule.vars,
        premises: rule.premises->Array.map(r => r->upshift(amount, ~from = from + len)),
        conclusion: rule.conclusion->Judgment.upshift(amount, ~from = from + len)
      }
    }
    type bare = { premises: array<t>, conclusion: Judgment.t }
    let instantiate = (rule: t, terms: array<Term.t>) => {
      assert(Array.length(terms) == Array.length(rule.vars))
      let terms' = [...terms]
      Array.reverse(terms')
      {
        premises: rule.premises->Array.map(r => r-> substDeBruijn(terms')),
        conclusion: rule.conclusion->Judgment.substDeBruijn(terms')
      }
    }
    
    let parseVinculum = (str) => {
      let re = RegExp.fromStringWithFlags(vinculumRES,~flags="y")
      switch re->RegExp.exec(str) {
      | None => Error("expected vinculum")
      | Some(res) => { switch res[1] {
        | Some(Some(n)) if String.trim(n) != "" 
            => Ok(n,String.sliceToEnd(str,~start=RegExp.lastIndex(re)))
        | _ => Ok("", String.sliceToEnd(str,~start=RegExp.lastIndex(re)))
        }}
      }
    }
    
    exception InternalParseError(string)
    let rec parseInner = (string, ~scope=[]: array<Term.meta>, ~gen=?) => {
      if (string->String.trim->String.get(0) == Some("[")) {
        let cur = ref(String.make(string->String.trim->String.sliceToEnd(~start=1)));
        let it = ref(Error(""))
        let vars = []
        while {it := Term.parseMeta(cur.contents); it.contents->Result.isOk} {
          let (str,rest) = Result.getExn(it.contents)
          Array.unshift(vars,str)
          cur := rest
        }
        let it = ref(Error(""))
        let premises = []
        switch {  
          while (cur.contents->String.trim->String.slice(~start=0,~end=2) != "|-" 
            && cur.contents->String.trim->String.get(0) != Some("]")) {
            switch parseInner(cur.contents,~scope=vars->Array.concat(scope), ~gen=?gen) {
            | Ok(p,rest) => {
                cur := rest
                premises->Array.push(p)
              }
            | Error(_) => raise(InternalParseError("expected turnstile or premise"))
            }
          }
          if (cur.contents->String.trim->String.get(0) == Some("]")) {
            let rest = cur.contents->String.trim->String.sliceToEnd(~start=1)
            cur := rest
            switch premises {
            | [{vars:[],premises:[],conclusion:e}] => Ok(({vars,premises:[],conclusion:e}, rest))
            | _ => Error("Conclusion appears to be multiple terms")
            }
          } else {
            cur := cur.contents->String.trim->String.sliceToEnd(~start=2)
            switch Judgment.parse(cur.contents,~scope=vars->Array.concat(scope),~gen=?gen) {
            | Ok(conclusion, rest) =>
              if (rest->String.trim->String.get(0) == Some("]")) {
                cur := rest->String.trim->String.sliceToEnd(~start=1);
                Ok(({vars, premises, conclusion}, cur.contents))
              } else {
                Error("Expected closing bracket")
              }
            | Error(e) => Error(e)
            }
          }
        } {
        | exception InternalParseError(e) => Error(e)
        | v => v
        }
      } else { 
        switch Judgment.parse(string,~scope,~gen=?gen) {
        | Ok(conclusion, rest) => Ok(({vars:[], premises:[], conclusion}, rest))
        | Error(e) => Error(e)
        }
      }
    }
    let parseTopLevel = (string, ~gen=?, ~scope=[]) => {
      let cur = ref(String.make(string));
      let it = ref(Error(""))
      let vars = []
      switch {
        while {it := Term.parseMeta(cur.contents); it.contents->Result.isOk} {
          let (str,rest) = Result.getExn(it.contents)
          Array.unshift(vars,str)
          cur := rest
        }
        let it = ref(Error(""))
        let premises = []
        while {it := parseVinculum(cur.contents); it.contents->Result.isError} {
          switch parseInner(cur.contents,~scope=vars->Array.concat(scope), ~gen=?gen) {
          | Ok(p,rest) => {
              cur := rest
              premises->Array.push(p)
            }
          | Error(e) => raise(InternalParseError(e))
          }
        }
        let (ruleName,rest) = it.contents->Result.getExn
        cur := rest
        switch Judgment.parse(cur.contents,~scope=vars->Array.concat(scope),~gen=?gen) {
        | Ok(conclusion, rest) => Ok((({vars, premises, conclusion}, ruleName), rest))
        | Error(e) => Error(e)
        }
      } {
      | exception InternalParseError(e) => Error(e)
      | v => v
      }
    }
    
    let rec prettyPrintInline = (rule: t,~scope=[]:array<Term.meta>) => {
      switch rule {
      | {vars: [], premises: [], conclusion: c} => Judgment.prettyPrint(c,~scope)
      | _ => {
        "["->String.concat(
          rule.vars
          ->Array.map(Term.prettyPrintMeta)
          ->Array.join("")
          ->String.concat(" ")
          ->String.concat(if Array.length(rule.premises) == 0 {
              Judgment.prettyPrint(rule.conclusion,~scope=[...rule.vars,...scope])
            } else {
              rule.premises
                ->Array.map(r => prettyPrintInline(r,~scope=[...rule.vars,...scope]))->Array.join(" ")
                ->String.concat(" |- ")
                ->String.concat(Judgment.prettyPrint(rule.conclusion,~scope=[...rule.vars,...scope]))
            }
          ))->String.concat("]")
        }
      }
    }
    let rec prettyPrintTopLevel = (rule: t,~name="",~scope=[]:array<Term.meta>) => {
      let vinculise = (strs: array<string>) => {
        let l = strs->Array.map(String.length)->Array.concat([4])->Math.Int.maxMany
        strs->Array.concat(["-"->String.repeat(l)->String.concat(" ")->String.concat(name)])
      }
      let myReverse = (arr) => {
        Array.reverse(arr)
        arr
      }
      rule.vars
        ->Array.map(Term.prettyPrintMeta)
        ->myReverse
        ->Array.join("")
        ->String.concat(newline)
        ->String.concat(
              rule.premises
                ->Array.map(r => prettyPrintInline(r,~scope=[...rule.vars,...scope]))
                ->vinculise
                ->Array.concat([Judgment.prettyPrint(rule.conclusion,~scope=[...rule.vars,...scope])])
                ->Array.map(s => String.concat("  ",s))
                ->Array.join(newline))
    }
  }
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

module FirstOrder = ProofEngine({
  module Term = SExp
  module Judgment = SExp
})
open FirstOrder

module TestProofs = Proof(Either(Deduction,Lemma))