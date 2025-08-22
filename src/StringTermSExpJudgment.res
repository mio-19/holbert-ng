type t = (StringTerm.t, string, SExp.t)
type substVal = StringV(StringTerm.t) | SExpV(SExp.t)
type subst = Map.t<int, substVal>
let mapSubst = Util.mapMapValues
let splitSub: subst => (StringTerm.subst, SExp.subst) = s => {
  let stringSub = Map.make()
  let judgeSub = Map.make()
  s->Map.forEachWithKey((v, k) => {
    switch v {
    | StringV(t) => stringSub->Map.set(k, t)
    | SExpV(t) => judgeSub->Map.set(k, t)
    }
  })
  (stringSub, judgeSub)
}
let substitute = ((term, m, judge): t, sub) => {
  let (stringSub, judgeSub) = splitSub(sub)
  (StringTerm.substitute(term, stringSub), m, SExp.substitute(judge, judgeSub))
}
let equivalent = ((t1, m1, j1): t, (t2, m2, j2): t) =>
  StringTerm.equivalent(t1, t2) && m1 == m2 && SExp.equivalent(j1, j2)
let unify = ((t1, m1, j1): t, (t2, m2, j2): t, ~gen=?) => {
  if m1 == m2 {
    // cartesian prod of possible unifications
    let judgeSubs = SExp.unify(j1, j2)->Seq.map(s => s->Util.mapMapValues(t => SExpV(t)))
    let stringSubs = StringTerm.unify(t1, t2)->Seq.map(s => s->Util.mapMapValues(t => StringV(t)))
    judgeSubs->Seq.flatMap(judgeSub =>
      // NOTE: silent failure mode here where substitution exists for a given schematic on both string
      // SExp side. for now, bias string sub. in future, maybe consider this not a valid judgement to begin with.
      // FIXME: we can just perform a naive conversion between strings and sexp
      stringSubs->Seq.map(stringSub => Util.mapUnion(stringSub, judgeSub))
    )
  } else {
    Seq.fromArray([])
  }
}
let substDeBruijn = ((t, m, j): t, scope: array<substVal>, ~from: int=0) => {
  // NOTE: again, silent failure here where variable has wrong type
  let stringScope = scope->Array.map(v =>
    switch v {
    | StringV(t) => t
    | _ => []
    }
  )
  let judgeScope = scope->Array.map(v =>
    switch v {
    | SExpV(t) => t
    | _ => SExp.Compound({subexps: []})
    }
  )
  (StringTerm.substDeBruijn(t, stringScope, ~from), m, SExp.substDeBruijn(j, judgeScope, ~from))
}
let upshift = ((t, m, j): t, amount: int, ~from: int=0) => (
  StringTerm.upshift(t, amount, ~from),
  m,
  SExp.upshift(j, amount, ~from),
)

let upshiftSubstVal = (v: substVal, amount: int, ~from: int=0) =>
  switch v {
  | StringV(t) => StringV(StringTerm.upshift(t, amount, ~from))
  | SExpV(j) => SExpV(SExp.upshift(j, amount, ~from))
  }

let parse = (str: string, ~scope: array<StringTerm.meta>, ~gen=?) => {
  StringTerm.parse(str, ~scope, ~gen?)->Result.flatMap(((t, str)) => {
    let re = RegExp.fromString(`^\\s*${Util.identRegexStr}`)
    switch Util.execRe(re, str) {
    | Some([m], l) => {
        let str = str->String.sliceToEnd(~start=l)
        SExp.parse(str, ~scope)->Result.map(((j, str)) => ((t, m, j), str))
      }
    | Some(_) => Error("judgement regex error")
    | None => Error("expected judgement identifier")
    }
  })
}
// TODO: figure out correct gen type
let parseSubstVal = (str: string, ~scope: array<StringTerm.meta>, ~gen=?) => {
  switch StringTerm.parse(str, ~scope, ~gen?) {
  | Ok(t, str) => Ok(StringV(t), str)
  | Error(stringE) =>
    switch SExp.parse(str, ~scope) {
    | Ok(t, str) => Ok(SExpV(t), str)
    | Error(sExpE) =>
      Error(
        `string or sexp expected.\nstring parsing failed with error: ${stringE}\nsexp parsing failed with error: ${sExpE}`,
      )
    }
  }
}

let prettyPrint = ((t, m, j): t, ~scope: array<StringTerm.meta>) =>
  `${StringTerm.prettyPrint(t, ~scope)} ${m} ${SExp.prettyPrint(j, ~scope)}`
let prettyPrintSubstVal = (v: substVal, ~scope: array<StringTerm.meta>) =>
  switch v {
  | StringV(t) => StringTerm.prettyPrint(t, ~scope)
  | SExpV(t) => SExp.prettyPrint(t, ~scope)
  }
