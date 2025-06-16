
open Signatures

module Context = (Term : TERM, Judgment : JUDGMENT with module Term := Term) => {
  module Rule = Rule.Make(Term,Judgment)
  type rec t = {
    fixes: array<Term.meta>, 
    facts: Dict.t<Rule.t>,
  }
}
let newline = "\n"
module type PROOF_METHOD = {
  module Term : TERM
  module Judgment : JUDGMENT with module Term := Term
  module Rule : module type of Rule.Make(Term,Judgment)
  module Context : module type of Context(Term,Judgment)
  type t<'a>
  let keywords : array<string>
  let check : (t<'a>, Context.t, Judgment.t, 
    ('a, Rule.t) => 'b) => result<t<'b>,string>
  let uncheck : (t<'a>, 'a => 'b) => t<'b>
  let parse : (string, ~keyword: string, ~scope: array<Term.meta>,~gen: Term.gen,
              ~subparser: (string, ~scope: array<Term.meta>, ~gen: Term.gen) 
                          => result<('a,string),string> )
          => result<(t<'a>,string),string>
  let prettyPrint : (t<'a>,~scope: array<Term.meta>,~indentation:int=?,
                    ~subprinter: ('a,~scope:array<Term.meta>,~indentation:int=?)
                    =>string) 
          => string
}


module Derivation = (
  Term : TERM, Judgment : JUDGMENT with module Term := Term
) => {  
  module Rule = Rule.Make(Term,Judgment)
  module Context = Context(Term,Judgment)
  type t<'a> = {
    ruleName: string,
    instantiation: array<Term.t>,
    subgoals: array<'a>
  }
  let uncheck = (it : t<'a>, f) => {
    { ruleName: it.ruleName, 
      instantiation: it.instantiation, 
      subgoals: it.subgoals->Array.map(f)
    }
  }
  exception InternalParseError(string)
  let keywords = ["by"]
  let prettyPrint = (it : t<'a>, ~scope,~indentation=0,
    ~subprinter: ('a,~scope:array<Term.meta>, ~indentation:int=?)=>string) => {
    let args = it.instantiation->Array.map(t => Term.prettyPrint(t,~scope));
    "by ("
      ->String.concat(Array.join([it.ruleName,...args]," "))
      ->String.concat(") {")
      ->String.concat(if Array.length(it.subgoals) > 0 { newline } else { "" })
      ->String.concat(it.subgoals
          ->Array.map(s => subprinter(s, ~scope,~indentation=indentation+2))
          ->Array.join(newline))
      ->String.concat("}")
  }
  let parse = (input, ~keyword as _, ~scope, ~gen, ~subparser) => {
    let cur = ref(String.trim(input))
    if (cur.contents->String.get(0) == Some("(")) {
      switch Rule.parseRuleName(cur.contents->String.sliceToEnd(~start=1)) {      
      | Ok((ruleName,rest)) => {
          cur := rest
          let instantiation = [];
          let it = ref(Error(""))
          while {it := Term.parse(cur.contents,~scope,~gen);
                 it.contents->Result.isOk} {
            let (val, rest) = it.contents->Result.getExn     
            Array.push(instantiation, val)
            cur := String.trim(rest)
          }
          if (cur.contents->String.get(0) == Some(")")) {
            cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
            let subgoals = []
            if (cur.contents->String.get(0) == Some("{")) {
              cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
              try {
                while (cur.contents->String.get(0) != Some("}")) {
                  switch subparser(cur.contents,~scope,~gen) {
                  | Ok((sg, rest)) => {
                      Array.push(subgoals,sg)
                      cur := String.trim(rest)
                    }
                  | Error(e) => raise(InternalParseError(e))
                  }
                } 
                if (cur.contents->String.get(0) == Some("}")) {
                  cur := String.trim(cur.contents->String.sliceToEnd(~start=1))
                  Ok(({ruleName,instantiation,subgoals},cur.contents))
                } else {
                  Error("} or subgoal proof expected")
                }
              } catch {
              | InternalParseError(e) => Error(e)
              }
            } else {
              Error("{ expected")  
            }
          } else {
            Error(") or term expected")
          }
        }
      | Error(e) => Error(e)
      }
    } else {
      Error("Expected (")
    }
  }
  let check = (it: t<'a>, ctx : Context.t, j: Judgment.t, f : ('a, Rule.t) => 'b) => {
    switch ctx.facts->Dict.get(it.ruleName) {
    | None => 
      Error("Cannot find rule '"->String.concat(it.ruleName)->String.concat("'") )
    | Some(rule) if Array.length(rule.vars) == Array.length(it.instantiation) => {
        let {premises,conclusion} = Rule.instantiate(rule,it.instantiation)
        if Judgment.equivalent(conclusion,j) {
          if Array.length(it.subgoals) == Array.length(premises) {
            Ok({
              ruleName: it.ruleName,
              instantiation: it.instantiation,
              subgoals: Belt.Array.zipBy(it.subgoals,premises,f)
            })
          } else {
            Error("Incorrect number of subgoals")
          }
        } else {
          let concString = Judgment.prettyPrint(conclusion,~scope=ctx.fixes)
          let goalString = Judgment.prettyPrint(j,~scope=ctx.fixes)
          Error("Conclusion of rule '"
            ->String.concat(concString)
            ->String.concat("' doesn't match goal '")
            ->String.concat(goalString)
            ->String.concat("'"))
        }
      } 
    | _ => Error("Incorrect number of binders")
    }
  }
}
module Lemma = (
  Term : TERM, Judgment : JUDGMENT with module Term := Term
) => {  
  module Rule = Rule.Make(Term,Judgment)
  module Context = Context(Term,Judgment)
  type t<'a> = {
    rule: Rule.t,
    proof: 'a,
    show: 'a
  }
  let uncheck = (it: t<'a>, f) => {
    { rule: it.rule,
      proof: f(it.proof),
      show: f(it.show)
    }
  }
  let keywords = ["have"]
  let prettyPrint = (it : t<'a>, ~scope,~indentation=0,
     ~subprinter: ('a,~scope:array<Term.meta>, ~indentation:int=?)=>string) => {
    "have "->String.concat(Rule.prettyPrintInline(it.rule,~scope))
      ->String.concat(newline)
      ->String.concat(subprinter(it.proof,~scope,~indentation))
      ->String.concat(newline)
      ->String.concat(subprinter(it.show,~scope,~indentation))
  }
  let parse = (input, ~keyword as _, ~scope, ~gen, ~subparser) => {
    //todo add toplevel
    switch Rule.parseInner(input, ~scope, ~gen) {
    | Ok((rule,rest)) => {
        switch (subparser(rest,~scope,~gen)) {
        | Ok((proof,rest')) => 
          switch String.trim(rest')->subparser(~scope,~gen) {
          | Ok((show,rest'')) =>  Ok({rule,proof,show}, rest'')
          | Error(e) => Error(e)
          }
        | Error(e) => Error(e)
        }
      }
    | Error(e) => Error(e)
    }
  }
  let check = (it: t<'a>, ctx : Context.t, j: Judgment.t, f : ('a, Rule.t) => 'b) => {
    let first = f(it.proof,it.rule)
    let second = f(it.show,{vars:[],premises:[it.rule],conclusion:j})
    Ok({rule:it.rule, proof:first, show:second})
  }
}
module Combine = (
  Term : TERM, Judgment : JUDGMENT with module Term := Term, 
  Method1 : PROOF_METHOD with module Term := Term and module Judgment := Judgment,
  Method2 : PROOF_METHOD with module Term := Term and module Judgment := Judgment
) => {
  module Rule = Rule.Make(Term,Judgment)
  module Context = Context(Term,Judgment)
  type t<'a> = First(Method1.t<'a>) | Second(Method2.t<'a>)
  let uncheck = (it, f) => switch it {
  | First(m) => First(Method1.uncheck(m,f))
  | Second(m) => Second(Method2.uncheck(m,f))
  }
  let keywords = Array.concat(Method1.keywords,Method2.keywords)
  
  let check = (it,ctx,  j, f) => switch it {
  | First(m) => m->Method1.check(ctx,j,f)->Result.map(x => First(x))
  | Second(m) => m->Method2.check(ctx,j,f)->Result.map(x => Second(x))
  }
  let prettyPrint = (it : t<'a>, ~scope, ~indentation=0,~subprinter) => switch it {
  | First(m) => m->Method1.prettyPrint(~scope,~indentation,~subprinter)
  | Second(m) => m->Method2.prettyPrint(~scope,~indentation,~subprinter)
  }
  let parse = (input, ~keyword, ~scope, ~gen, ~subparser) => {
    if Method1.keywords->Array.indexOf(keyword) > -1 {
      Method1.parse(input,~keyword,~scope,~gen,~subparser)->Result.map(((x,r)) => (First(x),r))
    } else {
      Method2.parse(input,~keyword,~scope,~gen,~subparser)->Result.map(((x,r)) => (Second(x),r))
    }
  }
}
module Proof = (
  Term : TERM, Judgment : JUDGMENT with module Term := Term, 
  Method : PROOF_METHOD with module Term := Term and module Judgment := Judgment
) => {  
  module Rule = Rule.Make(Term,Judgment)
  module Context = Context(Term,Judgment)
  type rec t = {
    fixes: array<Term.meta>, 
    assumptions: array<string>,
    method: option<Method.t<t>>
  }  
  type rec checked
    = Checked({
        fixes: array<Term.meta>, 
        assumptions: array<string>,
        method: option<Method.t<checked>>,
        rule: Rule.t
      })
    | ProofError({ raw: t, rule: Rule.t, msg: string})
  let parseKeyword = (input) => {
    Method.keywords
      ->Array.concat(["?"])
      ->Array.find(kw => String.trim(input)->String.startsWith(kw))
  }
  let rec prettyPrint = (prf : t, ~scope, ~indentation=0) => {
    let mtd = switch prf.method {
    | None => "?"
    | Some(m) => Method.prettyPrint(m,
        ~scope=prf.fixes->Array.concat(scope),
        ~indentation=indentation+2,
        ~subprinter=prettyPrint)
    }
    String.padStart("",indentation," ")
      ->String.concat(prf.fixes->Array.map(Term.prettyPrintMeta)->Array.join(""))
      ->String.concat(prf.assumptions
        ->Array.map(s => String.concat(" ", s))
        ->Array.join(""))
      ->String.concat(if Array.length(prf.assumptions) == 0 { 
          "|- " 
        } else { 
          " |- "
        })
      ->String.concat(mtd)
  }
  let rec parse = (input, ~scope,~gen) => {
    let it = ref(Error(""))
    let cur = ref(String.trim(input))
    let fixes = []
    while {it := Term.parseMeta(cur.contents); it.contents->Result.isOk} {
      let (n,r) = Result.getExn(it.contents)
      cur := String.trim(r)
      fixes->Array.unshift(n)
    }
    let it = ref(Error(""))
    let assumptions = []
    while {it := Rule.parseRuleName(cur.contents); it.contents->Result.isOk} {
      let (a,r) = Result.getExn(it.contents)
      cur := String.trim(r)
      assumptions->Array.push(a)
    }
    if cur.contents->String.slice(~start=0,~end=2) != "|-" {
      Console.log((fixes,assumptions))
      Error("expected turnstile or rule name"->String.concat(cur.contents))
    } else {
      cur := cur.contents->String.trim->String.sliceToEnd(~start=2)->String.trim
      let scope' = Array.concat(fixes, scope)
      switch parseKeyword(cur.contents) {
      | Some("?") => {
          Ok(({fixes,assumptions,method:None},
            cur.contents->String.sliceToEnd(~start=1)))
        }
      | Some(keyword) => {
          cur := cur.contents->String.sliceToEnd(~start=String.length(keyword))
          switch Method.parse(
            cur.contents, ~keyword,~scope=scope',~gen,~subparser=parse) {
          | Ok((method,r)) => Ok(({fixes,assumptions,method:Some(method)},r))
          | Error(e) => Error(e)
          }
        }
      | None => Error("Expected keyword")
      }
    }
  }
  let enter = (ctx : Context.t, prf: t, rule: Rule.t) => {
    if Array.length(prf.fixes) == Array.length(rule.vars) {
      if Array.length(prf.assumptions) == Array.length(rule.premises) {
        let newFacts = Dict.fromArray(Belt.Array.zip(prf.assumptions,rule.premises))
        Ok({ 
          Context.fixes: rule.vars->Array.concat(ctx.fixes),
          facts: Dict.copy(ctx.facts)->Dict.assign(newFacts)
        })
      } else {
        Error("Proof introduces a different number of assumptions than the rule")
      }
    } else {
      Error("Proof introduces a different number of variables than the rule")
    }
  
  } //result<Context, string>
  
  let rec uncheck = (prf : checked) => switch prf {
  | ProofError({ raw, rule:_, msg:_ }) => raw
  | Checked({fixes,assumptions,method,rule:_}) => {
      fixes, assumptions, 
      method: method->Option.map(xs => xs->Method.uncheck(uncheck))
    }
  }
  let rec check = (ctx : Context.t, prf: t, rule: Rule.t) => {
    let ruleStr = Rule.prettyPrintInline(rule,~scope=[])
    Console.log(("CHECK",ctx,prf,ruleStr))
    switch enter(ctx,prf,rule) {
    | Ok(ctx') => switch prf.method {
      | Some(m) =>
        switch m->Method.check(ctx', rule.conclusion, (s, r) => check(ctx',s,r)) {
        | Ok(m') => Checked({
            rule, fixes: prf.fixes, 
            assumptions: prf.assumptions, 
            method:Some(m')
          })
        | Error(e) => ProofError({raw:prf, rule, msg:e}) 
        }
      | None => Checked({
          rule, fixes: prf.fixes, assumptions: prf.assumptions, method: None
        })
      }
    | Error(e) => ProofError({raw:prf, rule, msg: e})
    }
  }// result<checked,string>

}

  /* 
  
    
  
  */
