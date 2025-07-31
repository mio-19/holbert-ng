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
type meta = string
type schematic = int
type subst = Map.t<schematic, t>
type subst1 = Belt.Map.Int.t<t>
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
  | Lam({name: _, body}) =>
    freeVarsIn(body)
    ->Belt.Set.toArray
    ->Array.filterMap(v =>
      if v >= 1 {
        Some(v - 1)
      } else {
        None
      }
    )
    ->Belt.Set.fromArray(~id=module(IntCmp))
  | App({func, arg}) => Belt.Set.union(freeVarsIn(func), freeVarsIn(arg))
  | _ => Belt.Set.make(~id=module(IntCmp))
  }
let rec mapbind = (term: t, f: int => int, ~from: int=0): t =>
  switch term {
  | Symbol(_) => term
  | Var({idx}) =>
    Var({
      idx: if idx >= from {
        let new = f(idx - from) + from
        if new < 0 {
          raise(Err("mapbind: negative index"))
        }
        new
      } else {
        idx
      },
    })
  | Schematic({schematic}) =>
    Schematic({
      schematic: schematic,
    })
  | Lam({name, body}) =>
    Lam({
      name,
      body: mapbind(body, f, ~from=from + 1),
    })
  | App({func, arg}) =>
    App({
      func: mapbind(func, f, ~from),
      arg: mapbind(arg, f, ~from),
    })
  }
let upshift = (term: t, amount: int, ~from: int=0) => mapbind(term, idx => idx + amount, ~from)
let downshift = (term: t, amount: int, ~from: int=1) => {
  if amount > from {
    raise(Err("downshift amount must be less than from"))
  }
  mapbind(term, idx => idx - amount, ~from)
}
let rec upshiftSubst = (subst: subst, amount: int, ~from: int=0) => {
  let nu = Map.make()
  Map.entries(subst)->Iterator.forEach(opt =>
    switch opt {
    | None => ()
    | Some((key, term)) => nu->Map.set(key, upshift(term, amount, ~from))
    }
  )
  nu
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
      body: substitute(body, upshiftSubst(subst, 1)),
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
  }
let rec lam = (amount: int, term: t): t => {
  assert(amount >= 0)
  if amount <= 0 {
    term
  } else {
    Lam({
      name: "",
      body: lam(amount - 1, term),
    })
  }
}
let rec app = (term: t, args: array<t>): t =>
  if args->Array.length == 0 {
    term
  } else {
    app(App({func: term, arg: args[0]->Option.getExn}), args->Array.sliceToEnd(~start=1))
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
let rec strip = (term: t): (t, array<t>) => {
  switch term {
  | App({func, arg}) =>
    let (peeledFunc, peeledArgs) = strip(func)
    (peeledFunc, [arg, ...peeledArgs])
  | _ => (term, [])
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
exception UnifyFail(string)
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
          combineSubst(
            subst,
            singletonSubst(
              schematic,
              lam(
                len,
                app(
                  Schematic({
                    schematic: h,
                  }),
                  hargs,
                ),
              ),
            ),
          )
        }
      | _ => raise(UnifyFail("not a symbol, var or schematic"))
      }
    }
  }
let rec unifyTerm = (a: t, b: t, ~gen: option<gen>) =>
  switch (reduce(a), reduce(b)) {
  | (Symbol({name: na}), Symbol({name: nb})) if na == nb => emptySubst
  | (Var({idx: ia}), Var({idx: ib})) if ia == ib => emptySubst
  | (Schematic({schematic: sa, _}), Schematic({schematic: sb, _})) if sa == sb => emptySubst
  | (Lam({name: _, body: ba}), Lam({name: _, body: bb})) => unifyTerm(ba, bb, ~gen)
  | (Lam({name: _, body: ba}), b) =>
    unifyTerm(ba, App({func: upshift(b, 1), arg: Var({idx: 0})}), ~gen)
  | (a, Lam({name: _, body: bb})) =>
    unifyTerm(App({func: upshift(a, 1), arg: Var({idx: 0})}), bb, ~gen)
  | (_, _) => cases(a, stripReduce(a), b, stripReduce(b), ~gen)
  }
and unifyArray = (a: array<(t, t)>, ~gen: option<gen>) => {
  if Array.length(a) == 0 {
    emptySubst
  } else {
    let (x, y) = a[0]->Option.getExn
    let s1 = unifyTerm(x, y, ~gen)
    let s2 =
      a
      ->Array.sliceToEnd(~start=1)
      ->Array.map(((t1, t2)) => (substitute(t1, s1), substitute(t2, s1)))
      ->unifyArray(~gen)
    combineSubst(s1, s2)
  }
}
and cases = (at: t, a: stripped, bt: t, b: stripped, ~gen: option<gen>) => {
  switch (a.func, b.func) {
  // rigid-rigid
  | (Symbol(_) | Var(_), Symbol(_) | Var(_)) =>
    if a.args->Array.length == b.args->Array.length && equivalent(a.func, b.func) {
      unifyArray(Belt.Array.zip(a.args, b.args), ~gen)
    } else {
      raise(UnifyFail("rigid-rigid mismatch"))
    }
  // flex-rigid
  | (Schematic({schematic}), Symbol(_) | Var(_)) =>
    if (
      // TODO: is this check strong enough to prevent cycles? do we need to check more
      !Belt.Set.has(schematicsIn(bt), schematic)
    ) {
      let map: array<option<int>> = a.args->Array.map(v =>
        switch v {
        | Var({idx}) => Some(idx)
        | _ => None
        }
      )
      let set = Belt.Set.fromArray(map->Array.keepSome, ~id=module(IntCmp))
      if (
        gen->Option.isNone ||
        map->Array.some(Option.isNone) ||
        set->Belt.Set.size < map->Array.length
      ) {
        raise(UnifyFail("flex-rigid mismatch"))
      } else {
        let i = a.args->Array.length
        let substV: substVar = Map.make()
        Array.reverse(map)
        map->Array.forEachWithIndex((v, i) =>
          switch v {
          | Some(idx) => substV->Map.set(idx + i, i)
          | None => raise(Unreachable("bug"))
          }
        )
        let allowed = Array.fromInitializer(~length=a.args->Array.length, i =>
          i
        )->Array.map(idx => Var({
          idx: idx,
        }))
        let hs: array<t> = Array.fromInitializer(~length=b.args->Array.length, _ =>
          app(
            Schematic({
              schematic: fresh(Option.getExn(gen)),
            }),
            allowed,
          )
        )
        // TODO: define a proj
        let s = unifyArray(
          Belt.Array.zip(b.args->Array.map(x => substVar(upshift(x, i), substV)), hs),
          ~gen,
        )
        let term: t = substitute(lam(i, app(b.func, hs)), s)
        combineSubst(s, singletonSubst(schematic, term))
      }
    } else {
      raise(UnifyFail("flex-rigid schematic already in use"))
    }
  | (Symbol(_) | Var(_), Schematic({schematic})) => cases(bt, b, at, a, ~gen)
  // flex-flex
  | (Schematic({schematic: sa}), Schematic({schematic: sb})) =>
    if equivalent(a.func, b.func) {
      if a.args->Array.length != b.args->Array.length {
        raise(UnifyFail("flex-flex mismatch: different number of args"))
      } else {
        // flex-flex same
        let len = a.args->Array.length
        let a_s: array<option<int>> = a.args->Array.map(v =>
          switch v {
          | Var({idx}) => Some(idx)
          | _ => None
          }
        )
        let b_s: array<option<int>> = b.args->Array.map(v =>
          switch v {
          | Var({idx}) => Some(idx)
          | _ => None
          }
        )
        let allowed =
          Belt.Array.zip(a_s, b_s)
          ->Array.mapWithIndex((pair, idx) =>
            switch pair {
            | (Some(a), Some(b)) if a == b => Some(len - idx - 1)
            | (_, _) => None
            }
          )
          ->Array.keepSome
          ->Array.map(idx => Var({idx: idx}))
        let h = app(
          Schematic({
            schematic: fresh(Option.getExn(gen)),
          }),
          allowed,
        )
        singletonSubst(sa, lam(len, h))
      }
    } else {
      // flex-flex different
      let lista = a.args->Array.filterMap(v =>
        switch v {
        | Var({idx}) => Some(idx)
        | _ => None
        }
      )
      let seta = Belt.Set.fromArray(lista, ~id=module(IntCmp))
      let common = b.args->Array.filterMap(v =>
        switch v {
        | Var({idx}) =>
          if Belt.Set.has(seta, idx) {
            Some(idx)
          } else {
            None
          }
        | _ => None
        }
      )
      let varmap = Map.make()
      common->Array.forEachWithIndex((v, idx) => varmap->Map.set(v, idx))
      let amap = Map.make()
      let a_len = a.args->Array.length
      a.args->Array.forEachWithIndex((v, idx) =>
        switch v {
        | Var({idx: i}) => amap->Map.set(i, a_len - idx - 1)
        | _ => ()
        }
      )
      let bmap = Map.make()
      let b_len = b.args->Array.length
      b.args->Array.forEachWithIndex((v, idx) =>
        switch v {
        | Var({idx: i}) => bmap->Map.set(i, b_len - idx - 1)
        | _ => ()
        }
      )
      let h = lam(
        common->Array.length,
        app(
          Schematic({
            schematic: fresh(Option.getExn(gen)),
          }),
          Array.fromInitializer(~length=common->Array.length, i => Var({idx: i})),
        ),
      )
      let a_args = common->Array.map(id => Var({idx: amap->Map.get(id)->Option.getExn}))
      let b_args = common->Array.map(id => Var({idx: bmap->Map.get(id)->Option.getExn}))
      combineSubst(singletonSubst(sa, app(h, a_args)), singletonSubst(sb, app(h, b_args)))
    }
  | (_, _) => raise(UnifyFail("not a schematic, symbol or var"))
  }
}
let unify = (a: t, b: t, ~gen=?) => {
  try {
    [unifyTerm(a, b, ~gen)]
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
