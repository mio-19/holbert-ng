type t = (StringTerm.t, string)
let substitute = ((term, m): t, sub) => (StringTerm.substitute(term, sub), m)
let equivalent = ((t1, m1): t, (t2, m2): t) => StringTerm.equivalent(t1, t2) && m1 == m2
let unify = ((t1, m1): t, (t2, m2): t) =>
  if m1 == m2 {
    StringTerm.unify(t1, t2)
  } else {
    []
  }
let substDeBruijn = ((t, m): t, scope: array<StringTerm.t>, ~from: int=0) => (
  StringTerm.substDeBruijn(t, scope, ~from),
  m,
)
let upshift = ((t, m): t, amount: int, ~from: int=0) => (StringTerm.upshift(t, amount, ~from), m)

let parse = (str: string, ~scope: array<StringTerm.meta>, ~gen=?) => {
  StringTerm.parse(str, ~scope, ~gen?)->Result.flatMap(((t, str)) => {
    let re = RegExp.fromString(`^\\s*${Util.identRegexStr}`)
    switch Util.execRe(re, str) {
    | Some([m], l) => Ok(((t, m), str->String.sliceToEnd(~start=l)))
    | Some(_) => Error("judgement regex error")
    | None => Error("expected judgement identifier")
    }
  })
}

let prettyPrint = ((t, m): t, ~scope: array<StringTerm.meta>) =>
  `${StringTerm.prettyPrint(t, ~scope)} ${m}`
