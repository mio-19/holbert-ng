open Util

type pos = {line: int, col: int, idx: int}
type located<'a> = {content: 'a, loc: pos}
type rec piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type t = array<piece>
type meta = string
type schematic = int
type subst = Map.t<schematic, t>

let substitute = (term: t, subst: subst) =>
  Array.flatMap(term, piece => {
    switch piece {
    | Schematic({schematic, _}) =>
      switch Map.get(subst, schematic) {
      | None => term
      | Some(found) => found
      }
    | _ => term
    }
  })
// TODO: fix
let unify: (t, t) => array<subst> = (s, t) => []
// law: unify(a,b) == [{}] iff equivalent(a,b)
let equivalent: (t, t) => bool = (s, t) => s == t
let substDeBruijn = (string: t, substs: array<t>, ~from: int=0) => {
  Array.flatMap(string, piece =>
    switch piece {
    | String(_) => [piece]
    | Var({idx: var}) =>
      if var < from {
        [piece]
      } else if var - from < Array.length(substs) && var - from >= 0 {
        Option.getUnsafe(substs[var - from])
      } else {
        [Var({idx: var - Array.length(substs)})]
      }
    | Schematic({schematic, allowed}) => [
        Schematic({
          schematic,
          allowed: Array.filterMap(allowed, i =>
            if i < from + Array.length(substs) {
              None
            } else {
              Some(i - (from + Array.length(substs)))
            }
          ),
        }),
      ]
    }
  )
}

let upshift = (term: t, amount: int, ~from: int=0) =>
  Array.map(term, piece => {
    switch piece {
    | String(_) => piece
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
    }
  })

let place = (x: int, ~scope: array<string>) => [
  Schematic({
    schematic: x,
    allowed: Array.fromInitializer(~length=Array.length(scope), i => i),
  }),
]

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
let makeGen = () => {
  ref(0)
}

let parseMeta = (str: string) => {
  let re = %re("/^([^\s.\[\]()]+)\./y")
  switch re->RegExp.exec(str->String.trim) {
  | None => Error("not a meta name")
  | Some(res) =>
    switch RegExp.Result.matches(res) {
    | [n] => Ok(n, String.sliceToEnd(str->String.trim, ~start=RegExp.lastIndex(re)))
    | _ => Error("impossible happened")
    }
  }
}
let prettyPrintVar = (idx: int, scope: array<string>) =>
  switch scope[idx] {
  | Some(n) if Array.indexOf(scope, n) == idx && false => n
  | _ => "\\"->String.concat(String.make(idx))
  }
let prettyPrint = (term: t, ~scope: array<string>) =>
  Array.map(term, piece => {
    switch piece {
    | String(str) => `"${str}"`
    | Var({idx}) => prettyPrintVar(idx, scope)
    | Schematic({schematic, allowed}) => {
        let allowedStr =
          allowed
          ->Array.map(idx => prettyPrintVar(idx, scope))
          ->Array.join(" ")
        `?${Int.toString(schematic)}(${allowedStr})`
      }
    }
  })->Array.join(".")
let prettyPrintMeta = (str: string) => `${str}.`
type token =
  | StringLit(string)
  | VarLit(int)
  | SchemaLit({id: int, allowed: array<int>})
  | Concat
  | LParen
  | RParen
type remaining = string
type errorMessage = string
let parse: (string, ~scope: array<meta>, ~gen: gen=?) => result<(t, string), string> = (
  str: string,
  ~scope: array<string>,
  ~gen=?,
) => {
  let error = (loc: pos, msg: string) => {
    let codeAroundLoc = String.slice(str, ~start=loc.idx, ~end=loc.idx + 5)
    Error(`${Int.toString(loc.line)}:${Int.toString(loc.col)} (${codeAroundLoc})...: ${msg}`)
  }

  let execRe = (re, str) => {
    re
    ->RegExp.exec(str)
    ->Option.map(result => {
      open RegExp.Result
      (matches(result), fullMatch(result)->String.length)
    })
  }

  let lex: unit => result<array<located<token>>, string> = () => {
    let pos = ref(0)
    let line = ref(1)
    let col = ref(1)
    let loc = () => {
      line: line.contents,
      col: col.contents,
      idx: pos.contents,
    }
    let acc = ref(Ok([]))
    let advance = n => {
      pos := pos.contents + n
      col := col.contents + n
    }
    let advance1 = () => advance(1)
    let newline = () => {
      pos := pos.contents + 1
      line := line.contents + 1
      col := 1
    }
    let add = (token, ~nAdvance=?) => {
      acc.contents
      ->Result.map(acc => {
        Array.push(
          acc,
          {
            content: token,
            loc: {
              line: line.contents,
              col: col.contents,
              idx: pos.contents,
            },
          },
        )
      })
      ->ignore
      Option.map(nAdvance, advance)->ignore
    }
    let error = msg => {
      Console.log(acc.contents->Result.getExn)
      acc := error(loc(), msg)
    }
    let execRe = re => execRe(re, String.sliceToEnd(str, ~start=pos.contents))
    let stringLit = () => {
      let regex = %re("/^\"([^\"]*)\"/")
      switch execRe(regex) {
      | Some([match], l) => add(StringLit(match), ~nAdvance=l)
      | Some(_) => error("regex string lit error")
      | None => error("expected end quote")
      }
    }
    let readInt = s => Int.fromString(s)->Option.getExn
    let varLit = () => {
      let varRegex = %re("/^\\(\d+)/")
      let schemaRegex = %re("/\\\?(\d+)\(((?:\d+\s*)*)\)/")
      switch execRe(varRegex) {
      | Some([match], l) => add(VarLit(readInt(match)), ~nAdvance=l)
      | Some(_) => error("var lit regex error")
      | None =>
        switch execRe(schemaRegex) {
        | Some([idStr, allowedStr], l) => {
            let id = readInt(idStr)
            let allowed =
              allowedStr
              ->String.trim
              ->String.splitByRegExp(%re("/\s+/"))
              ->Array.keepSome
              ->Array.filter(s => s != "")
              ->Array.map(readInt)
            add(SchemaLit({id, allowed}), ~nAdvance=l)
          }
        | Some(_) => error("schema lit regex error")
        | None => error("expected var or schema")
        }
      }
    }
    let varScope = () => {
      let varRegex = %re("/^([a-zA-Z]\w*)/")
      switch execRe(varRegex) {
      | Some([ident], l) =>
        switch Array.indexOfOpt(scope, ident) {
        | Some(idx) => add(VarLit(idx), ~nAdvance=l)
        | None => error("expected variable in scope")
        }
      | Some(_) => error("var regex error")
      | None => error("unexpected token")
      }
    }
    while {pos.contents < String.length(str) && Result.isOk(acc.contents)} {
      switch String.get(str, pos.contents)->Option.getExn {
      | " " | "\t" | "\r" => advance1()
      | "\n" => newline()
      | "\"" => stringLit()
      | "." => add(Concat, ~nAdvance=1)
      | "(" => add(LParen, ~nAdvance=1)
      | ")" => add(RParen, ~nAdvance=1)
      | "\\" => varLit()
      | _ => varScope()
      }
    }
    acc.contents
  }
  let parseExp = (tokens: array<located<token>>) => {
    let tokenIdx = ref(0)
    let acc: ref<result<array<piece>, (errorMessage, remaining)>> = ref(Ok([]))
    let parens = []
    let add = p => {acc.contents->Result.map(acc => acc->Array.push(p))->ignore}

    while {tokenIdx.contents < Array.length(tokens) && Result.isOk(acc.contents)} {
      let {content: tok, loc} = tokens[tokenIdx.contents]->Option.getExn
      let error = msg => {
        acc :=
          error(loc, msg)->Result.mapError(msg => (msg, String.sliceToEnd(str, ~start=loc.idx)))
      }
      switch tok {
      | StringLit(s) => add(String(s))
      | SchemaLit({id: schematic, allowed}) => add(Schematic({schematic, allowed}))
      | VarLit(idx) => add(Var({idx: idx})) // some reason i'm not allowed to shorthand here?
      | LParen => Array.push(parens, loc)
      | RParen =>
        if Array.length(parens) == 0 {
          error("no matching open paren")
        } else {
          Array.pop(parens)->ignore
        }
      | Concat =>
        switch tokens[tokenIdx.contents + 1] {
        | None | Some({content: RParen}) => error("expected string to follow concatenation")
        | Some(c) => ()
        }
      }
      tokenIdx := tokenIdx.contents + 1
    }
    switch acc.contents {
    | Ok(t) =>
      if Array.length(parens) == 0 {
        Ok(t, "")
      } else {
        let loc = Array.pop(parens)->Option.getExn
        error(loc, "expected closing paren")
      }
    // TODO: decide if this remaining is redundant
    | Error(msg, remaining) => Error(msg)
    }
  }
  switch lex() {
  | Ok(tokens) => parseExp(tokens)
  | Error(msg) => Error(msg)
  }
}
