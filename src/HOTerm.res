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
  | Unit
type meta = string
type schematic = int
type subst = Belt.Map.Int.t<t>
let substHas = (subst: subst, schematic: schematic) => subst->Belt.Map.Int.has(schematic)
let substGet = (subst: subst, schematic: schematic) => subst->Belt.Map.Int.get(schematic)
let mapMapValues = (m: subst, f: t => t): subst => {
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
  | _ => Belt.Set.make(~id=module(IntCmp))
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
  | _ => Belt.Set.make(~id=module(IntCmp))
  }
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
  | Unit => Unit
  }
let mapbind = (term: t, f: int => int, ~from: int=0): t => mapbind0(term, idx => Ok(f(idx)), ~from)
let upshift = (term: t, amount: int, ~from: int=0) => mapbind(term, idx => idx + amount, ~from)
let downshift = (term: t, amount: int, ~from: int=1) => {
  if amount > from {
    raise(Err("downshift amount must be less than from"))
  }
  mapbind(term, idx => idx - amount, ~from)
}
type substVar = Map.t<int, int>
let incrSubstVar = (subst: substVar) => {
  let nu = Map.make()
  Map.entries(subst)->Iterator.forEach(opt =>
    switch opt {
    | None => ()
    | Some((key, value)) => nu->Map.set(key + 1, value + 1)
    }
  )
  nu
}
let rec substVar = (term: t, subst: substVar) =>
  switch term {
  | Symbol(_) => term
  | Var({idx}) =>
    switch Map.get(subst, idx) {
    | None => term
    | Some(newIdx) => Var({idx: newIdx})
    }
  | Schematic({schematic}) =>
    Schematic({
      schematic: schematic,
    })
  | Lam({name, body}) =>
    Lam({
      name,
      body: substVar(body, incrSubstVar(subst)),
    })
  | App({func, arg}) =>
    App({
      func: substVar(func, subst),
      arg: substVar(arg, subst),
    })
  | Unit => Unit
  }
let emptySubst: subst = Belt.Map.Int.empty
let substAdd = (subst: subst, schematic: schematic, term: t) => {
  assert(schematic >= 0)
  assert(subst->Belt.Map.Int.has(schematic) == false)
  subst->Belt.Map.Int.set(schematic, term)
}
let singletonSubst = (schematic: schematic, term: t): subst => {
  assert(schematic >= 0)
  emptySubst->substAdd(schematic, term)
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
      body: substitute(body, subst),
    })
  | App({func, arg}) =>
    App({
      func: substitute(func, subst),
      arg: substitute(arg, subst),
    })
  | _ => term
  }
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
  | Unit => Unit
  }
let rec lamn = (amount: int, term: t): t => {
  assert(amount >= 0)
  if amount <= 0 {
    term
  } else {
    Lam({
      name: "",
      body: lamn(amount - 1, term),
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
let var = (idx: int): t => {
  assert(idx >= 0)
  Var({idx: idx})
}
let idx1 = (is: array<int>, j: int): t => {
  switch idx(is->Array.map(var), j) {
  | None => Unit
  | Some(idx) => Var({idx: idx})
  }
}
let idx2 = (is: array<t>, j: int): result<int, int => t> => {
  switch idx(is, j) {
  | None => Error(_ => Unit)
  | Some(idx) => Ok(idx)
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
let rec red = (term: t, is: array<t>, js: array<t>): t => {
  switch term {
  | Lam({name, body}) if is->Array.length > 0 => {
      let head = is[0]->Option.getExn
      let rest = is->Array.sliceToEnd(~start=1)
      red(body, rest, Array.concat([head], js))
    }
  | _ =>
    app(
      term->mapbind(k =>
        switch js[k + 1]->Option.getExn {
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
let lam = (is: array<int>, g: t, js: array<int>): t => {
  lamn(is->Array.length, app(g, js->Array.map(j => idx1(is, j))))
}
type stripped = {
  func: t,
  args: array<t>,
}
// only reduce the outermost application
let rec reduce = (term: t) => {
  switch term {
  | App({func, arg}) =>
    switch reduce(func) {
    | Lam({body}) => reduce(substDeBruijn(body, [arg]))
    | func => App({func, arg})
    }
  | term => term
  }
}
let rec reduceFull = (term: t): t => {
  switch term {
  | App({func, arg}) =>
    switch reduce(func) {
    | Lam({body}) => reduce(substDeBruijn(body, [arg]))
    | func => App({func, arg: reduceFull(arg)})
    }
  | Lam({name, body}) =>
    Lam({
      name,
      body: reduceFull(body),
    })
  | Symbol(_) | Var(_) | Schematic(_) => term

  | Unit => Unit
  }
}
let rec strip = (term: t): (t, array<t>) => {
  switch term {
  | App({func, arg}) =>
    let (peeledFunc, peeledArgs) = strip(func)
    (peeledFunc, [arg, ...peeledArgs])
  | _ => (term, [])
  }
}
let rec devar = (subst: subst, term: t): t => {
  let (func, args) = strip(term)
  switch func {
  | Schematic({schematic}) if substHas(subst, schematic) =>
    devar(subst, app1(substGet(subst, schematic)->Option.getExn, args))
  | _ => term
  }
}
let rec stripReduce = (term: t): stripped => {
  switch reduce(term) {
  | App({func, arg}) =>
    let {func: peeledFunc, args: peeledArgs} = stripReduce(func)
    {func: peeledFunc, args: [arg, ...peeledArgs]}
  | _ => {func: term, args: []}
  }
}
let rec proj = (allowed: array<int>, term: t, ~gen: option<gen>, ~subst: subst=emptySubst): subst =>
  switch reduce(term) {
  | Lam({name: _, body}) =>
    proj(Array.concat([0], allowed->Array.map(x => x + 1)), body, ~gen, ~subst)
  | term => {
      let a = stripReduce(term)
      switch a.func {
      | Symbol(_) => Array.reduce(a.args, subst, (acc, a) => proj(allowed, a, ~gen, ~subst=acc))
      | Var({idx}) =>
        if allowed->Array.some(v => v == idx) {
          Array.reduce(a.args, subst, (acc, a) => proj(allowed, a, ~gen, ~subst=acc))
        } else {
          raise(UnifyFail("variable not in allowed set"))
        }
      | Schematic({schematic}) => {
          let len = a.args->Array.length
          let hargs =
            a.args
            ->Array.mapWithIndex((b, idx) =>
              switch b {
              | Var({idx: x}) if allowed->Array.some(v => v == x) => Some(idx)
              | _ => None
              }
            )
            ->Array.keepSome
            ->Array.map(idx => len - idx - 1)
            ->Array.map(idx => Var({idx: idx}))
          if gen->Option.isNone {
            raise(UnifyFail("no gen provided"))
          }
          let h = fresh(Option.getExn(gen))
          subst->substAdd(
            schematic,
            lamn(
              len,
              app1(
                Schematic({
                  schematic: h,
                }),
                hargs,
              ),
            ),
          )
        }
      | _ => raise(UnifyFail("not a symbol, var or schematic"))
      }
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
    let h = fresh(Option.getExn(gen))
    let xs = Belt.Array.init(len, k => {
      let a = xs[k]->Option.getExn
      let b = ys[k]->Option.getExn
      switch (a, b) {
      | (Var(_), Var(_)) if equivalent(a, b) => Some(Var({idx: len - k - 1}))
      | _ => None
      }
    })->Array.keepSome
    subst->substAdd(sa, lamn(len, app1(Schematic({schematic: h}), xs)))
  } else {
    raise(TODO("TODO"))
  }
}
let flexrigid = (sa: schematic, xs: array<t>, b: t, subst: subst, ~gen: option<gen>): subst => {
  if occ(sa, subst, b) {
    raise(UnifyFail("flexible schematic occurs in rigid term"))
  }
  let u = b->mapbind0(bind => idx2(xs, bind))
  raise(TODO("TODO"))
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
  | Schematic({schematic}) => "?"->String.concat(String.make(schematic))
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
  | Unit => ""
  }
let symbolRegexpString = "^([^\\s()]+)"
let nameRES = "^([^\\s.\\[\\]()]+)\\."
exception ParseError(string)
type token = LParen | RParen | VarT(int) | SchematicT(int) | AtomT(string) | DotT | EOF
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
    | "." => (DotT, rest())
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
          // -1: leave the dot in the string for the next DotT token
          | [n] => (AtomT(n), String.sliceToEnd(str, ~start=RegExp.lastIndex(reName) - 1))
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
      let abstract = switch t1 {
      | AtomT(_) =>
        let (t2, rest2) = tokenize(rest1)
        switch t2 {
        | DotT => true
        | _ => false
        }
      | _ => false
      }
      if abstract {
        let (t2, rest2) = tokenize(rest1)
        let name = switch t1 {
        | AtomT(n) => n
        | _ => raise(Unreachable("bug"))
        }
        let (result, rest3) = parseSimple("("->String.concat(rest2))
        (LambdaS({name, body: result}), rest3)
      } else {
        switch t1 {
        | RParen => (ListS({xs: []}), rest1)
        | _ => {
            let (head, rest2) = parseSimple(rest)
            let (tail, rest3) = parseSimple("("->String.concat(rest2))
            switch tail {
            | ListS({xs}) => (ListS({xs: [head, ...xs]}), rest3)
            | _ => raise(Unreachable("bug"))
            }
          }
        }
      }
    }
  | RParen => raise(ParseError("unexpected right parenthesis"))
  | VarT(idx) => (VarS({idx: idx}), rest)
  | SchematicT(schematic) => (SchematicS({schematic: schematic}), rest)
  | AtomT(name) => (AtomS({name: name}), rest)
  | DotT => raise(ParseError("unexpected dot"))
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
