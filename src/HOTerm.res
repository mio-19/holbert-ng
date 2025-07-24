open Util
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

type rec t =
  | Symbol({name: string})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
  | Lam({name: string, body: t})
  | App({func: t, arg: t})
type meta = string
type schematic = int
type subst = Map.t<schematic, t>
let rec equivalent = (a: t, b: t) => {
  switch (a, b) {
  | (Symbol({name: na}), Symbol({name: nb})) => na == nb
  | (Var({idx: ia}), Var({idx: ib})) => ia == ib
  | (Schematic({schematic: sa, allowed: aa}), Schematic({schematic: sb, allowed: ab})) =>
    sa == sb && Array.equal(aa, ab, (a, b) => a == b)
  | (Lam({name: _, body: ba}), Lam({name: _, body: bb})) => equivalent(ba, bb)
  | (App({func: fa, arg: aa}), App({func: fb, arg: ab})) => equivalent(fa, fb) && equivalent(aa, ab)
  | (_, _) => false
  }
}
let rec schematicsIn: t => Belt.Set.t<int, IntCmp.identity> = (it: t) =>
  switch it {
  | Schematic({schematic, _}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(schematic)
  | Lam({body}) => schematicsIn(body)
  | App({func, arg}) => Belt.Set.union(schematicsIn(func), schematicsIn(arg))
  | _ => Belt.Set.make(~id=module(IntCmp))
  }
let rec freeVarsIn: t => Belt.Set.t<int, IntCmp.identity> = (it: t) =>
  switch it {
  | Var({idx}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(idx)
  | Lam({name: _, body}) => freeVarsIn(body)
  | App({func, arg}) => Belt.Set.union(freeVarsIn(func), freeVarsIn(arg))
  | _ => Belt.Set.make(~id=module(IntCmp))
  }
let rec substitute = (term: t, subst: subst) =>
  switch term {
  | Schematic({schematic, _}) =>
    switch Map.get(subst, schematic) {
    | None => term
    | Some(found) => found
    }
  | Lam({name, body}) =>
    Lam({
      name,
      body: substitute(body, subst),
    })
  | App({func, arg}) =>
    App({
      func: substitute(func, subst),
      arg: substitute(arg, subst),
    })
  | _ => term
  }

let combineSubst = (s: subst, t: subst) => {
  let nu = Map.make()
  Map.entries(s)->Iterator.forEach(opt =>
    switch opt {
    | None => ()
    | Some((key, term)) => nu->Map.set(key, term->substitute(t))
    }
  )
  Map.entries(t)->Iterator.forEach(opt =>
    switch opt {
    | None => ()
    | Some((key, term)) => nu->Map.set(key, term->substitute(s))
    }
  )
  nu
}
let emptySubst: subst = Map.make()
let singletonSubst: (int, t) => subst = (schematic, term) => {
  let s = Map.make()
  s->Map.set(schematic, term)
  s
}
let rec upshift = (term: t, amount: int, ~from: int=0) =>
  switch term {
  | Symbol(_) => term
  | Var({idx}) =>
    Var({
      idx: if idx >= from {
        idx + amount
      } else {
        idx
      },
    })
  | Schematic({schematic, allowed}) =>
    Schematic({
      schematic,
      allowed: Array.map(allowed, i =>
        if i >= from {
          i + amount
        } else {
          i
        }
      ),
    })
  | Lam({name, body}) =>
    Lam({
      name,
      body: upshift(body, amount, ~from),
    })
  | App({func, arg}) =>
    App({
      func: upshift(func, amount, ~from),
      arg: upshift(arg, amount, ~from),
    })
  }
// returns {func: t, args: array<t>}
type peelAppT = {
  func: t,
  args: array<t>,
}
let rec peelApp = (term: t): peelAppT => {
  switch term {
  | App({func, arg}) =>
    let {func: peeledFunc, args: peeledArgs} = peelApp(func)
    {func: peeledFunc, args: [arg, ...peeledArgs]}
  | _ => {func: term, args: []}
  }
}
let reduce = (term: t, ~from: int) => {
  switch term {
  | App({func: Lam({body}), arg}) => substitute(body, singletonSubst(from, arg))
  | term => term
  }
}
let rec unifyTerm = (a: t, b: t, ~from: int) =>
  switch (reduce(a, ~from), reduce(b, ~from)) {
  | (Symbol({name: na}), Symbol({name: nb})) if na == nb => Some(emptySubst)
  | (Var({idx: ia}), Var({idx: ib})) if ia == ib => Some(emptySubst)
  | (Schematic({schematic: sa, _}), Schematic({schematic: sb, _})) if sa == sb => Some(emptySubst)
  | (Lam({name: _, body: ba}), Lam({name: _, body: bb})) => unifyTerm(ba, bb, ~from=from + 1)
  | (Lam({name: _, body: ba}), b) => unifyTerm(ba, upshift(b, 1, ~from), ~from=from + 1)
  | (a, Lam({name: _, body: bb})) => unifyTerm(upshift(a, 1, ~from), bb, ~from=from + 1)
  | (_, _) => cases(peelApp(a), peelApp(b), ~from)
  }
and unifyArray = (a: array<(t, t)>, ~from: int) => {
  if Array.length(a) == 0 {
    Some(emptySubst)
  } else {
    let (x, y) = a[0]->Option.getUnsafe
    switch unifyTerm(x, y, ~from) {
    | None => None
    | Some(s1) =>
      switch a
      ->Array.sliceToEnd(~start=1)
      ->Array.map(((t1, t2)) => (substitute(t1, s1), substitute(t2, s1)))
      ->unifyArray(~from) {
      | None => None
      | Some(s2) => Some(combineSubst(s1, s2))
      }
    }
  }
}
and cases = (a: peelAppT, b: peelAppT, ~from: int) => {
  switch (a.func, b.func) {
  // rigid-rigid
  | (Symbol(_) | Var(_), Symbol(_) | Var(_)) =>
    if a.args->Array.length == b.args->Array.length && equivalent(a.func, b.func) {
      unifyArray(Belt.Array.zip(a.args, b.args), ~from)
    } else {
      None
    }
  // rigid-flex
  | (Symbol(_) | Var(_), Schematic({schematic, allowed})) => raise(TODO("TODO"))
  | (Schematic({schematic, allowed}), Symbol(_) | Var(_)) => cases(b, a, ~from)
  | (_, _) => raise(TODO("TODO"))
  }
}
let unify = (a: t, b: t) => {
  switch unifyTerm(a, b, ~from=0) {
  | None => []
  | Some(s) => [s]
  }
}
let rec substDeBruijn = (term: t, substs: array<t>, ~from: int=0) =>
  switch term {
  | Symbol(_) => term
  | Var({idx: var}) =>
    if var < from {
      term
    } else if var - from < Array.length(substs) && var - from >= 0 {
      Option.getUnsafe(substs[var - from])
    } else {
      Var({idx: var - Array.length(substs)})
    }
  | Schematic({schematic, allowed}) =>
    Schematic({
      schematic,
      allowed: Array.filterMap(allowed, i =>
        if i < from + Array.length(substs) {
          None
        } else {
          Some(i - (from + Array.length(substs)))
        }
      ),
    })
  | Lam({name, body}) =>
    Lam({
      name,
      body: substDeBruijn(body, substs, ~from),
    })
  | App({func, arg}) =>
    App({
      func: substDeBruijn(func, substs, ~from),
      arg: substDeBruijn(arg, substs, ~from),
    })
  }
let place = (x: int, ~scope: array<string>) => Schematic({
  schematic: x,
  allowed: Array.fromInitializer(~length=Array.length(scope), i => i),
})
type gen = ref<int>
let seen = (g: gen, s: int) => {
  if s >= g.contents {
    g := s + 1
  }
}
let fresh = (g: gen, ~replacing as _=?) => {
  let v = g.contents
  g := g.contents + 1
  v
}
let prettyPrintVar = (idx: int, scope: array<string>) =>
  switch scope[idx] {
  | Some(n) if Array.indexOf(scope, n) == idx && false => n
  | _ => "\\"->String.concat(String.make(idx))
  }
let makeGen = () => {
  ref(0)
}
let rec prettyPrint = (it: t, ~scope: array<string>) =>
  switch it {
  | Symbol({name}) => name
  | Var({idx}) => prettyPrintVar(idx, scope)
  | Schematic({schematic, allowed}) =>
    "?"
    ->String.concat(String.make(schematic))
    ->String.concat("(")
    ->String.concat(Array.join(allowed->Array.map(idx => prettyPrintVar(idx, scope)), " "))
    ->String.concat(")")
  | Lam({name, body}) =>
    "(lambda "
    ->String.concat(name)
    ->String.concat(" ")
    ->String.concat(prettyPrint(body, ~scope=scope->Array.concat([name])))
    ->String.concat(")")
  | App({func, arg}) =>
    "("
    ->String.concat(prettyPrint(func, ~scope))
    ->String.concat(" ")
    ->String.concat(prettyPrint(arg, ~scope))
    ->String.concat(")")
  }
let symbolRegexpString = "^([^\\s()]+)"
let varRegexpString = "^\\\\([0-9]+)$"
let schematicRegexpString = "^\\?([0-9]+)$"
type lexeme = LParen | RParen | VarT(int) | SymbolT(string) | SchematicT(int)
let nameRES = "^([^\\s.\\[\\]()]+)\\."
let prettyPrintMeta = (str: string) => {
  String.concat(str, ".")
}
let parseMeta = (str: string) => {
  let re = RegExp.fromStringWithFlags(nameRES, ~flags="y")
  switch re->RegExp.exec(str->String.trim) {
  | None => Error("not a meta name")
  | Some(res) =>
    switch RegExp.Result.matches(res) {
    | [n] => Ok(n, String.sliceToEnd(str->String.trim, ~start=RegExp.lastIndex(re)))
    | _ => Error("impossible happened")
    }
  }
}
let parse = (str: string, ~scope: array<string>, ~gen=?) => {
  raise(TODO("parse not implemented"))
}
