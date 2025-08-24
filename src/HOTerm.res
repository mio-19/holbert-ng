open Util
module IntCmp = Belt.Id.MakeComparable({
  type t = int
  let cmp = Pervasives.compare
})

type rec t =
  | Symbol({name: string})
  | Var({idx: int})
  | Schematic({schematic: int})
  | Lam({name: string, body: t})
  | App({func: t, arg: t})
  // Unallowed is used internally in unify, where Nipkow 1993 uses Var(-infinity)
  | Unallowed
type meta = string
type schematic = int
type subst = Belt.Map.Int.t<t>
let substHas = (subst: subst, schematic: schematic) => subst->Belt.Map.Int.has(schematic)
let substGet = (subst: subst, schematic: schematic) => subst->Belt.Map.Int.get(schematic)
let mapSubst = (m: subst, f: t => t): subst => {
  m->Belt.Map.Int.map(f)
}
let rec equivalent = (a: t, b: t) => {
  switch (a, b) {
  | (Symbol({name: na}), Symbol({name: nb})) => na == nb
  | (Var({idx: ia}), Var({idx: ib})) => ia == ib
  | (Schematic({schematic: sa}), Schematic({schematic: sb})) => sa == sb
  | (Lam({name: _, body: ba}), Lam({name: _, body: bb})) => equivalent(ba, bb)
  | (App({func: fa, arg: aa}), App({func: fb, arg: ab})) => equivalent(fa, fb) && equivalent(aa, ab)
  | (_, _) => false
  }
}
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
let rec schematicsIn = (subst: subst, it: t): Belt.Set.t<schematic, IntCmp.identity> =>
  switch it {
  | Schematic({schematic, _}) if subst->substHas(schematic) =>
    let found = subst->substGet(schematic)->Option.getExn
    schematicsIn(subst, found)
  | Schematic({schematic, _}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(schematic)
  | Lam({body}) => schematicsIn(subst, body)
  | App({func, arg}) => Belt.Set.union(schematicsIn(subst, func), schematicsIn(subst, arg))
  | Unallowed | Symbol(_) | Var(_) => Belt.Set.make(~id=module(IntCmp))
  }
let occ = (schematic: schematic, subst: subst, t: t): bool => {
  let set = schematicsIn(subst, t)
  set->Belt.Set.has(schematic)
}
let rec freeVarsIn = (subst: subst, it: t): Belt.Set.t<int, IntCmp.identity> =>
  switch it {
  | Schematic({schematic, _}) if subst->substHas(schematic) =>
    let found = subst->substGet(schematic)->Option.getExn
    freeVarsIn(subst, found)
  | Var({idx}) => Belt.Set.make(~id=module(IntCmp))->Belt.Set.add(idx)
  | Lam({name: _, body}) =>
    freeVarsIn(subst, body)
    ->Belt.Set.toArray
    ->Array.filterMap(v =>
      if v >= 1 {
        Some(v - 1)
      } else {
        None
      }
    )
    ->Belt.Set.fromArray(~id=module(IntCmp))
  | App({func, arg}) => Belt.Set.union(freeVarsIn(subst, func), freeVarsIn(subst, arg))
  | Unallowed | Symbol(_) | Schematic(_) => Belt.Set.make(~id=module(IntCmp))
  }
let freeVarsContains = (term: t, subst: subst, idx: int): bool => {
  let set = freeVarsIn(subst, term)
  set->Belt.Set.has(idx)
}
// f might map an index to a new one or to a term. when mapping to a term the real index without shifting is passed
let rec mapbind0 = (term: t, f: int => result<int, int => t>, ~from: int=0): t =>
  switch term {
  | Symbol(_) => term
  | Var({idx}) =>
    if idx >= from {
      switch f(idx - from) {
      | Ok(newIdx) =>
        let new = newIdx + from
        if new < 0 {
          raise(Err("mapbind: negative index"))
        }
        Var({
          idx: new,
        })
      | Error(t) => t(from)
      }
    } else {
      term
    }
  | Schematic({schematic}) =>
    Schematic({
      schematic: schematic,
    })
  | Lam({name, body}) =>
    Lam({
      name,
      body: mapbind0(body, f, ~from=from + 1),
    })
  | App({func, arg}) =>
    App({
      func: mapbind0(func, f, ~from),
      arg: mapbind0(arg, f, ~from),
    })
  | Unallowed => Unallowed
  }
let mapbind = (term: t, f: int => int, ~from: int=0): t => mapbind0(term, idx => Ok(f(idx)), ~from)
let upshift = (term: t, amount: int, ~from: int=0) => mapbind(term, idx => idx + amount, ~from)
let downshift = (term: t, amount: int, ~from: int=1) => {
  if amount > from {
    raise(Err("downshift amount must be less than from"))
  }
  mapbind(term, idx => idx - amount, ~from)
}
let emptySubst: subst = Belt.Map.Int.empty
let substAdd = (subst: subst, schematic: schematic, term: t) => {
  assert(schematic >= 0)
  assert(subst->Belt.Map.Int.has(schematic) == false)
  subst->Belt.Map.Int.set(schematic, term)
}
let rec substitute = (term: t, subst: subst) =>
  switch term {
  | Schematic({schematic, _}) =>
    switch Belt.Map.Int.get(subst, schematic) {
    | None => term
    | Some(found) => found
    }
  | Lam({name, body}) =>
    Lam({
      name,
      // upshift is not needed for pattern unification, but it is safer to have upshift here
      body: substitute(body, subst->Belt.Map.Int.map(t => upshift(t, 1))),
    })
  | App({func, arg}) =>
    App({
      func: substitute(func, subst),
      arg: substitute(arg, subst),
    })
  | Var(_) | Unallowed | Symbol(_) => term
  }
// TODO: check how will this interact with meta variables (schematics) and check if it is needed to have a subst parameter - it should not be needed for subst produced by pattern unification
let rec substDeBruijn = (term: t, substs: array<t>, ~from: int=0) =>
  switch term {
  | Symbol(_) => term
  | Var({idx: var}) =>
    if var < from {
      term
    } else if var - from < Array.length(substs) && var - from >= 0 {
      Option.getExn(substs[var - from])
    } else {
      Var({idx: var - Array.length(substs)})
    }
  | Schematic({schematic}) =>
    Schematic({
      schematic: schematic,
    })
  | Lam({name, body}) =>
    Lam({
      name,
      body: substDeBruijn(body, substs->Array.map(term => upshift(term, 1)), ~from=from + 1),
    })
  | App({func, arg}) =>
    App({
      func: substDeBruijn(func, substs, ~from),
      arg: substDeBruijn(arg, substs, ~from),
    })
  | Unallowed => Unallowed
  }
// beta reduced and eta reduced
let rec reduceFull = (term: t, subst: subst): t => {
  switch term {
  | Schematic({schematic}) if subst->substHas(schematic) =>
    let found = subst->substGet(schematic)->Option.getExn
    reduceFull(found, subst)
  | Lam({body: App({func, arg: Var({idx: 0})})}) if !(func->freeVarsContains(subst, 0)) =>
    reduceFull(downshift(func, 1), subst)
  | App({func, arg}) =>
    switch reduceFull(func, subst) {
    | Lam({body}) => reduceFull(substDeBruijn(body, [arg]), subst)
    | func => App({func, arg: reduceFull(arg, subst)})
    }
  | Lam({name, body}) =>
    Lam({
      name,
      body: reduceFull(body, subst),
    })
  | Symbol(_) | Var(_) | Schematic(_) => term

  | Unallowed => Unallowed
  }
}
let reduceSubst = (subst: subst): subst => {
  subst->Belt.Map.Int.map(x => reduceFull(x, subst))
}
let rec lams = (amount: int, term: t): t => {
  assert(amount >= 0)
  if amount <= 0 {
    term
  } else {
    Lam({
      name: "x",
      body: lams(amount - 1, term),
    })
  }
}
let rec idx = (is: array<t>, j: int): option<int> => {
  if is->Array.length == 0 {
    None
  } else {
    let head = is[0]->Option.getExn
    let tail = is->Array.sliceToEnd(~start=1)
    if equivalent(head, Var({idx: j})) {
      Some(tail->Array.length)
    } else {
      idx(tail, j)
    }
  }
}
let idx1 = (is: array<t>, j: int): t => {
  switch idx(is, j) {
  | None => Unallowed
  | Some(idx) => Var({idx: idx})
  }
}
let idx2 = (is: array<t>, j: int): result<int, int => t> => {
  switch idx(is, j) {
  | None => Error(_ => Unallowed)
  | Some(idx) => Ok(idx)
  }
}
let lookup = (term: t, subst: array<(t, t)>): option<t> => {
  subst
  ->Array.find(((from, _)) => equivalent(term, from))
  ->Option.map(((_, to)) => to)
}
let rec discharge = (subst: array<(t, t)>, term: t): t => {
  switch lookup(term, subst) {
  | Some(found) => found
  | None =>
    switch term {
    | App({func, arg}) => App({func: discharge(subst, func), arg: discharge(subst, arg)})
    | Lam({name, body}) => Lam({name, body: discharge(subst, body)})
    | Var(_) | Schematic(_) | Symbol(_) | Unallowed => term
    }
  }
}
let rec app = (term: t, args: array<t>): t => {
  if args->Array.length == 0 {
    term
  } else {
    let head = args[0]->Option.getExn
    let rest = args->Array.sliceToEnd(~start=1)
    app(App({func: term, arg: head}), rest)
  }
}
exception UnifyFail(string)
let isvar = (term: t): bool =>
  switch term {
  | Var(_) => true
  | _ => false
  }
let rec red = (term: t, is: array<t>, js: array<t>): t => {
  switch term {
  | Lam({name, body}) if is->Array.length > 0 && isvar(is[0]->Option.getExn) => {
      let head = is[0]->Option.getExn
      let rest = is->Array.sliceToEnd(~start=1)
      red(body, rest, Array.concat([head], js))
    }
  | _ =>
    app(
      term->mapbind(k =>
        switch js[k]->Option.getExn {
        | Var({idx}) => idx
        | _ => raise(UnifyFail("expected variable in red"))
        }
      ),
      is,
    )
  }
}
// app with beta reduction
let rec app1 = (term: t, args: array<t>): t =>
  if args->Array.length == 0 {
    term
  } else {
    let head = args[0]->Option.getExn
    let rest = args->Array.sliceToEnd(~start=1)
    switch term {
    | Lam({body}) => app1(substDeBruijn(body, [head]), rest)
    | _ => app1(App({func: term, arg: head}), rest)
    }
  }
let lam = (is: array<t>, g: t, js: array<int>): t => {
  lams(is->Array.length, app(g, js->Array.map(j => idx1(is, j))))
}
let rec strip = (term: t): (t, array<t>) => {
  switch term {
  | App({func, arg}) =>
    let (peeledFunc, peeledArgs) = strip(func)
    (peeledFunc, Array.concat(peeledArgs, [arg]))
  | _ => (term, [])
  }
}
let rec devar = (subst: subst, term: t): t => {
  let (func, args) = strip(term)
  switch func {
  | Schematic({schematic}) if substHas(subst, schematic) =>
    devar(subst, red(substGet(subst, schematic)->Option.getExn, args, []))
  | _ => term
  }
}
let rec proj = (subst: subst, term: t, ~gen: option<gen>): subst => {
  switch strip(devar(subst, term)) {
  | (Lam({name, body}), args) if args->Array.length == 0 => proj(subst, body, ~gen)
  | (Unallowed, args) => raise(UnifyFail("unallowed"))
  | (Symbol(_) | Var(_), args) => Array.reduce(args, subst, (acc, a) => proj(acc, a, ~gen))
  | (Schematic({schematic}), args) => {
      assert(!substHas(subst, schematic))
      if gen->Option.isNone {
        raise(UnifyFail("no gen provided"))
      }
      let h = Schematic({schematic: fresh(Option.getExn(gen))})
      subst->substAdd(
        schematic,
        lams(
          args->Array.length,
          app(
            h,
            Belt.Array.init(args->Array.length, j => {
              switch args[j]->Option.getExn {
              | Var({idx}) => Some(Var({idx: args->Belt.Array.length - j - 1}))
              | _ => None
              }
            })->Array.keepSome,
          ),
        ),
      )
    }
  | _ => raise(UnifyFail("not a symbol, var or schematic"))
  }
}
let flexflex = (
  sa: schematic,
  xs: array<t>,
  sb: schematic,
  ys: array<t>,
  subst: subst,
  ~gen: option<gen>,
): subst => {
  if sa == sb {
    if xs->Array.length != ys->Array.length {
      raise(UnifyFail("flexible schematics have different number of arguments"))
    }
    if gen->Option.isNone {
      raise(UnifyFail("no gen provided"))
    }
    let len = xs->Array.length
    let h = Schematic({schematic: fresh(Option.getExn(gen))})
    let xs = Belt.Array.init(len, k => {
      let a = xs[k]->Option.getExn
      let b = ys[k]->Option.getExn
      switch (a, b) {
      | (Var(_), Var(_)) if equivalent(a, b) => Some(Var({idx: len - k - 1}))
      | _ => None
      }
    })->Array.keepSome
    subst->substAdd(sa, lams(len, app(h, xs)))
  } else {
    let y_vars = Belt.Set.fromArray(
      ys->Array.filterMap(y =>
        switch y {
        | Var({idx}) => Some(idx)
        | _ => None
        }
      ),
      ~id=module(IntCmp),
    )
    let common = xs->Array.filterMap(x =>
      switch x {
      | Var({idx}) if y_vars->Belt.Set.has(idx) => Some(idx)
      | _ => None
      }
    )
    let h = Schematic({schematic: fresh(Option.getExn(gen))})
    subst->substAdd(sa, lam(xs, h, common))->substAdd(sb, lam(ys, h, common))
  }
}
let flexrigid = (sa: schematic, xs: array<t>, b: t, subst: subst, ~gen: option<gen>): subst => {
  if occ(sa, subst, b) {
    raise(UnifyFail("flexible schematic occurs in rigid term"))
  }
  let u = b->mapbind0(bind => idx2(xs, bind))
  proj(subst->substAdd(sa, lams(xs->Array.length, u)), u, ~gen)
}
let rec unifyTerm = (a: t, b: t, subst: subst, ~gen: option<gen>): subst =>
  switch (devar(subst, a), devar(subst, b)) {
  | (Symbol({name: na}), Symbol({name: nb})) =>
    if na == nb {
      subst
    } else {
      raise(UnifyFail("symbols do not match"))
    }
  | (Var({idx: ia}), Var({idx: ib})) =>
    if ia == ib {
      subst
    } else {
      raise(UnifyFail("variables do not match"))
    }
  | (Schematic({schematic: sa}), Schematic({schematic: sb})) if sa == sb => subst
  | (Lam({name: _, body: ba}), Lam({name: _, body: bb})) => unifyTerm(ba, bb, subst, ~gen)
  | (Lam({name: _, body: ba}), b) =>
    unifyTerm(ba, App({func: upshift(b, 1), arg: Var({idx: 0})}), subst, ~gen)
  | (a, Lam({name: _, body: bb})) =>
    unifyTerm(App({func: upshift(a, 1), arg: Var({idx: 0})}), bb, subst, ~gen)
  | (_, _) =>
    switch (strip(a), strip(b)) {
    | ((Schematic({schematic: sa}), xs), (Schematic({schematic: sb}), ys)) =>
      flexflex(sa, xs, sb, ys, subst, ~gen)
    | ((Schematic({schematic: sa}), xs), _) => flexrigid(sa, xs, b, subst, ~gen)
    | (_, (Schematic({schematic: sb}), ys)) => flexrigid(sb, ys, a, subst, ~gen)
    | ((a, xs), (b, ys)) => rigidrigid(a, xs, b, ys, subst, ~gen)
    | (_, _) => raise(UnifyFail("no rules match"))
    }
  }
and unifyArray = (xs: array<t>, ys: array<t>, subst: subst, ~gen: option<gen>): subst => {
  if xs->Array.length != ys->Array.length {
    raise(UnifyFail("arrays have different lengths"))
  }
  Belt.Array.zip(xs, ys)->Belt.Array.reduce(subst, (acc, (x, y)) => unifyTerm(x, y, acc, ~gen))
}
and rigidrigid = (
  a: t,
  xs: array<t>,
  b: t,
  ys: array<t>,
  subst: subst,
  ~gen: option<gen>,
): subst => {
  if !equivalent(a, b) {
    raise(UnifyFail("rigid terms do not match"))
  }
  if xs->Array.length != ys->Array.length {
    raise(UnifyFail("rigid terms have different number of arguments"))
  }
  unifyArray(xs, ys, subst, ~gen)
}
let unify = (a: t, b: t, ~gen=?) => {
  try {
    [unifyTerm(a, b, emptySubst, ~gen)]
  } catch {
  | UnifyFail(_) => []
  }
}
let place = (x: int, ~scope: array<string>) => Schematic({
  schematic: x,
})
let prettyPrintVar = (idx: int, scope: array<string>) =>
  switch scope[idx] {
  | Some(n) if Array.indexOf(scope, n) == idx => n
  | _ => "\\"->String.concat(String.make(idx))
  }
let makeGen = () => {
  ref(0)
}
let rec stripLam = (it: t): (array<string>, t) =>
  switch it {
  | Lam({name, body}) =>
    let (names, body) = stripLam(body)
    (Array.concat([name], names), body)
  | _ => ([], it)
  }
let rec prettyPrint = (it: t, ~scope: array<string>) =>
  switch it {
  | Symbol({name}) => name
  | Var({idx}) => prettyPrintVar(idx, scope)
  | Schematic({schematic}) => "?"->String.concat(String.make(schematic))
  | Lam(_) =>
    let (names, body) = stripLam(it)
    let (func, args) = strip(body)
    let bodies = Array.concat([func], args)
    let innerScope = Array.concat(Array.toReversed(names), scope)
    "("
    ->String.concat(Array.join(names->Array.map(name => String.concat(name, ".")), " "))
    ->String.concat(" ")
    ->String.concat(Array.join(bodies->Array.map(e => prettyPrint(e, ~scope=innerScope)), " "))
    ->String.concat(")")
  | App(_) =>
    let (func, args) = strip(it)
    "("
    ->String.concat(prettyPrint(func, ~scope))
    ->String.concat(" ")
    ->String.concat(Array.join(args->Array.map(e => prettyPrint(e, ~scope)), " "))
    ->String.concat(")")
  | Unallowed => ""
  }
let symbolRegexpString = "^([^\\s()]+)"
let nameRES = "^([^\\s.\\[\\]()]+)\\."
exception ParseError(string)
type token = LParen | RParen | VarT(int) | SchematicT(int) | AtomT(string) | NameT(string) | EOF
let varRegexpString = "^\\\\([0-9]+)"
let schematicRegexpString = "^\\?([0-9]+)"
let tokenize = (str0: string): (token, string) => {
  let str = str0->String.trimStart
  if str->String.length == 0 {
    (EOF, "")
  } else {
    let rest = () => str->String.sliceToEnd(~start=1)
    switch str->String.charAt(0) {
    | "(" => (LParen, rest())
    | ")" => (RParen, rest())
    | "\\" => {
        let re = RegExp.fromStringWithFlags(varRegexpString, ~flags="y")
        switch re->RegExp.exec(str) {
        | None => raise(ParseError("invalid variable"))
        | Some(res) =>
          switch RegExp.Result.matches(res) {
          | [n] => (
              VarT(n->Int.fromString->Option.getExn),
              String.sliceToEnd(str, ~start=RegExp.lastIndex(re)),
            )
          | _ => raise(ParseError("invalid variable"))
          }
        }
      }
    | "?" => {
        let re = RegExp.fromStringWithFlags(schematicRegexpString, ~flags="y")
        switch re->RegExp.exec(str) {
        | None => raise(ParseError("invalid schematic"))
        | Some(res) =>
          switch RegExp.Result.matches(res) {
          | [n] => (
              SchematicT(n->Int.fromString->Option.getExn),
              String.sliceToEnd(str, ~start=RegExp.lastIndex(re)),
            )
          | _ => raise(ParseError("invalid schematic"))
          }
        }
      }
    | _ => {
        let reName = RegExp.fromStringWithFlags(nameRES, ~flags="y")
        switch reName->RegExp.exec(str) {
        | Some(res) =>
          switch RegExp.Result.matches(res) {
          | [n] => (NameT(n), String.sliceToEnd(str, ~start=RegExp.lastIndex(reName)))
          | _ => raise(ParseError("invalid symbol"))
          }
        | None =>
          let re = RegExp.fromStringWithFlags(symbolRegexpString, ~flags="y")
          switch re->RegExp.exec(str) {
          | None => raise(ParseError("invalid symbol"))
          | Some(res) =>
            switch RegExp.Result.matches(res) {
            | [n] => (AtomT(n), String.sliceToEnd(str, ~start=RegExp.lastIndex(re)))
            | _ => raise(ParseError("invalid symbol"))
            }
          }
        }
      }
    }
  }
}
type rec simple =
  | ListS({xs: array<simple>})
  | AtomS({name: string})
  | VarS({idx: int})
  | SchematicS({schematic: int})
  | LambdaS({name: string, body: simple})
let rec parseSimple = (str: string): (simple, string) => {
  let (t0, rest) = tokenize(str)
  switch t0 {
  | LParen => {
      let (t1, rest1) = tokenize(rest)
      switch t1 {
      | NameT(name) => {
          let (result, rest2) = parseSimple("("->String.concat(rest1))
          (LambdaS({name, body: result}), rest2)
        }
      | RParen => (ListS({xs: []}), rest1)
      | _ => {
          let (head, rest2) = parseSimple(rest)
          let (tail, rest3) = parseSimple("("->String.concat(rest2))
          switch tail {
          | ListS({xs}) => (ListS({xs: Array.concat([head], xs)}), rest3)
          | _ => raise(Unreachable("bug"))
          }
        }
      }
    }
  | RParen => raise(ParseError("unexpected right parenthesis"))
  | VarT(idx) => (VarS({idx: idx}), rest)
  | SchematicT(schematic) => (SchematicS({schematic: schematic}), rest)
  | AtomT(name) => (AtomS({name: name}), rest)
  | NameT(name) => {
      let (result, rest1) = parseSimple(rest)
      (LambdaS({name, body: result}), rest1)
    }
  | EOF => raise(ParseError("unexpected end of file"))
  }
}
type env = Map.t<string, int>
let incrEnv = (env: env): env => {
  let nu: env = Map.make()
  Map.entries(env)->Iterator.forEach(opt =>
    switch opt {
    | None => ()
    | Some((key, value)) => nu->Map.set(key, value + 1)
    }
  )
  nu
}
let envFromScope = (scope: array<string>): env => {
  let nu: env = Map.make()
  scope->Array.forEachWithIndex((name, idx) => {
    nu->Map.set(name, idx)
  })
  nu
}
let envPushLambda = (env: env, name: string): env => {
  let nu = incrEnv(env)
  nu->Map.set(name, 0)
  nu
}
let rec parseAll = (simple: simple, ~env: env, ~gen=?): t => {
  switch simple {
  | ListS({xs}) => {
      let ts = xs->Array.map(x => parseAll(x, ~env, ~gen?))
      if ts->Array.length == 0 {
        raise(ParseError("empty list"))
      } else {
        ts
        ->Array.sliceToEnd(~start=1)
        ->Array.reduce(ts[0]->Option.getExn, (acc, x) => App({func: acc, arg: x}))
      }
    }
  | AtomS({name}) =>
    if env->Map.has(name) {
      let idx = env->Map.get(name)->Option.getExn
      Var({idx: idx})
    } else {
      Symbol({name: name})
    }
  | VarS({idx}) => Var({idx: idx})
  | SchematicS({schematic}) =>
    switch gen {
    | Some(g) => {
        seen(g, schematic)
        Schematic({schematic: schematic})
      }
    | None => raise(ParseError("Schematics not allowed here"))
    }
  | LambdaS({name, body}) =>
    Lam({
      name,
      body: parseAll(body, ~env=envPushLambda(env, name), ~gen?),
    })
  }
}
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
  try {
    let (simple, rest) = parseSimple(str)
    Ok((parseAll(simple, ~env=envFromScope(scope), ~gen?), rest))
  } catch {
  | ParseError(msg) => Error(msg)
  }
}
