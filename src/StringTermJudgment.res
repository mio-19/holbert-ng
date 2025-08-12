type t = (StringTerm.t, string, SExp.t)
type substVal = StringV(StringTerm.t) | SExpV(SExp.t)
type subst = Map.t<int, substVal>
let mapSubst = Util.mapMapValues
let liftSubst: StringTerm.subst => subst = s => Util.mapMapValues(s, v => StringV(v))
let splitSub: subst => (StringTerm.subst, SExp.subst) = s => Error("todo")->Result.getExn
let substitute = ((term, m, judge): t, sub) => {
  let (stringSub, judgeSub) = splitSub(sub)
  (StringTerm.substitute(term, stringSub), m, SExp.substitute(judge, judgeSub))
}
let equivalent = ((t1, m1, j1): t, (t2, m2, j2): t) => StringTerm.equivalent(t1, t2) && m1 == m2
let unify = ((t1, m1, j1): t, (t2, m2, j2): t) => {
  Error("todo")->Result.getExn
}
// if m1 == m2 {
//   let judgeSubs = SExp.unify(j1, j2)
//   StringTerm.unify(t1, t2)->Array.flatMap(stringSub =>
//     judgeSubs->Array.map(judgeSub => (stringSub, judgeSub))
//   )
// } else {
//   []
// }
let substDeBruijn = ((t, m, j): t, scope: array<substVal>, ~from: int=0) => {
  Error("todo")->Result.getExn
}
// (
//   StringTerm.substDeBruijn(t, scope, ~from),
//   m,
//   j,
// )
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
        SExp.parse(str, ~scope=[])->Result.map(((j, str)) => ((t, m, j), str))
      }
    | Some(_) => Error("judgement regex error")
    | None => Error("expected judgement identifier")
    }
  })
}
let parseSubstVal = (str: string, ~scope: array<StringTerm.meta>, ~gen=?) =>
  Error("todo")->Result.getExn

let prettyPrint = ((t, m, j): t, ~scope: array<StringTerm.meta>) =>
  `${StringTerm.prettyPrint(t, ~scope)} ${m} ${SExp.prettyPrint(j, ~scope=[])}`
let prettyPrintSubstVal: (substVal, ~scope: array<StringTerm.meta>) => string =
  Error("todo")->Result.getExn
