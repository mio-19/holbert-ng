module StringSymbol: SExpFunc.SYMBOL = {
  type t = StringS(StringTerm.t) | ConstS(SExp.ConstSymbol.t)
  type subst = Map.t<int, t>
  type gen = ref<int>
  let parse = (s, ~scope, ~gen: option<gen>=?) => {
    StringTerm.parse(s, ~scope, ~gen?)
    ->Result.map(((r, rest)) => (StringS(r), rest))
    ->Util.Result.or(() =>
      SExp.ConstSymbol.parse(s, ~scope, ~gen?)->Result.map(((r, rest)) => (ConstS(r), rest))
    )
  }
  let prettyPrint = (s, ~scope) =>
    switch s {
    | StringS(s) => StringTerm.prettyPrint(s, ~scope)
    | ConstS(s) => SExp.ConstSymbol.prettyPrint(s, ~scope)
    }
  let unify = (s1, s2) =>
    switch (s1, s2) {
    | (StringS(s1), StringS(s2)) =>
      StringTerm.unify(s1, s2)->Seq.map(subst => subst->Util.mapMapValues(v => StringS(v)))
    | (ConstS(s1), ConstS(s2)) =>
      SExp.ConstSymbol.unify(s1, s2)->Seq.map(subst => subst->Util.mapMapValues(v => ConstS(v)))
    | (_, _) => Seq.empty
    }
  let substitute = (s, subst: subst) =>
    switch s {
    | StringS(s) => {
        let stringSubs = subst->Util.mapMapValues(v =>
          switch v {
          | StringS(s) => s
          | _ => throw(Util.Unreachable("const should not have map values"))
          }
        )
        StringS(StringTerm.substitute(s, stringSubs))
      }
    | ConstS(s) => ConstS(s)
    }
}
module StringSExp = SExpFunc.Make(StringSymbol)

let toSExp = (t: StringTerm.t): StringSExp.t => {
  let convertPiece = (p: StringTerm.piece): StringSExp.t =>
    switch p {
    | StringTerm.String(s) => {
        let (s, _) = StringSymbol.parse(s, ~scope=[])->Result.getExn
        StringSExp.Symbol(s)
      }
    | StringTerm.Var({idx}) => StringSExp.Var({idx: idx})
    | StringTerm.Schematic({schematic, allowed}) => StringSExp.Schematic({schematic, allowed})
    | StringTerm.Ghost => StringSExp.ghostTerm
    }
  switch Array.length(t) {
  | 1 => convertPiece(t[0]->Option.getExn)
  | _ => StringSExp.Compound({subexps: Array.map(t, convertPiece)})
  }
}

let rec fromSExp = (t: StringSExp.t): StringTerm.t =>
  switch t {
  | StringSExp.Symbol(name) => [StringTerm.String(StringSymbol.prettyPrint(name, ~scope=[]))]
  | StringSExp.Schematic({schematic, allowed}) => [StringTerm.Schematic({schematic, allowed})]
  | StringSExp.Var({idx}) => [StringTerm.Var({idx: idx})]
  | StringSExp.Compound({subexps}) => subexps->Array.flatMap(fromSExp)
  | StringSExp.Ghost => [StringTerm.Ghost]
  }

type t = (StringTerm.t, StringSExp.t)
type substCodom = StringV(StringTerm.t) | SExpV(StringSExp.t)
type subst = Map.t<int, substCodom>
type schematic = int
type meta = string

let mapSubst = Util.mapMapValues
let mergeSubsts: (subst, subst) => subst = Util.mapUnion
let splitSub: subst => (StringTerm.subst, StringSExp.subst) = s => {
  let stringSub = Map.make()
  let judgeSub = Map.make()
  s->Map.forEachWithKey((v, k) => {
    switch v {
    | StringV(t) => {
        stringSub->Map.set(k, t)
        judgeSub->Map.set(k, t->toSExp)
      }
    | SExpV(t) => {
        stringSub->Map.set(k, t->fromSExp)
        judgeSub->Map.set(k, t)
      }
    }
  })
  (stringSub, judgeSub)
}
let substitute = ((term, judge): t, sub) => {
  let (stringSub, judgeSub) = splitSub(sub)
  (StringTerm.substitute(term, stringSub), StringSExp.substitute(judge, judgeSub))
}

let substituteSubstCodom = (s: substCodom, subst: subst) => {
  let (stringSub, judgeSub) = splitSub(subst)
  switch s {
  | StringV(t) => StringV(StringTerm.substitute(t, stringSub))
  | SExpV(t) => SExpV(StringSExp.substitute(t, judgeSub))
  }
}
let equivalent = ((t1, j1): t, (t2, j2): t) =>
  StringTerm.equivalent(t1, t2) && StringSExp.equivalent(j1, j2)
let reduce = t => t
let unify = ((t1, j1): t, (t2, j2): t, ~gen as _=?) => {
  // cartesian prod of possible unifications
  let judgeSubs = StringSExp.unify(j1, j2)->Seq.map(s => s->Util.mapMapValues(t => SExpV(t)))
  let stringSubs = StringTerm.unify(t1, t2)->Seq.map(s => s->Util.mapMapValues(t => StringV(t)))
  judgeSubs->Seq.flatMap(judgeSub =>
    // NOTE: silent failure mode here where substitution exists for a given schematic on both string
    // SExp side. for now, bias string sub. in future, maybe consider this not a valid judgement to begin with.
    stringSubs->Seq.map(stringSub => Util.mapUnion(stringSub, judgeSub))
  )
}
let substDeBruijn = ((t, j): t, scope: array<substCodom>, ~from: int=0) => {
  // NOTE: implicit type coercion here. if we unify and expect a string but get an sexp,
  // perform naive flattening of compound to substitute. likewise in opposite direction.
  let stringScope = scope->Array.map(v =>
    switch v {
    | StringV(t) => t
    | SExpV(t) => fromSExp(t)
    }
  )
  let judgeScope = scope->Array.map(v =>
    switch v {
    | SExpV(t) => t
    | StringV(t) => toSExp(t)
    }
  )
  (StringTerm.substDeBruijn(t, stringScope, ~from), StringSExp.substDeBruijn(j, judgeScope, ~from))
}
let upshift = ((t, j): t, amount: int, ~from: int=0) => (
  StringTerm.upshift(t, amount, ~from),
  StringSExp.upshift(j, amount, ~from),
)

let upshiftSubstCodom = (v: substCodom, amount: int, ~from: int=0) =>
  switch v {
  | StringV(t) => StringV(StringTerm.upshift(t, amount, ~from))
  | SExpV(j) => SExpV(StringSExp.upshift(j, amount, ~from))
  }

let mapTerms = ((t, j): t, f: StringTerm.t => StringTerm.t): t => {
  // Apply the function to both the string term and the sexp (converted to/from string term)
  let newT = f(t)
  // FIX: is this necessary? is mapTerms even used?
  let newJ = j->fromSExp->f->toSExp
  (newT, newJ)
}

let parse = (str: string, ~scope: array<string>, ~gen: option<ref<int>>=?) => {
  StringTerm.parse(str, ~scope, ~gen?)->Result.flatMap(((t, str)) =>
    StringSExp.parse(str, ~scope)->Result.map(((j, str)) => ((t, j), str))
  )
}

// HACK: this does work due to the hacky string-sexp conversion we have inplace with substitutions,
// but a different solution would be preferable
let placeSubstCodom = (x: int, ~scope: array<string>) => StringV(StringTerm.place(x, ~scope))

let parseSubstCodom = (str: string, ~scope: array<StringTerm.meta>, ~gen=?) => {
  switch StringTerm.parse(str, ~scope, ~gen?) {
  | Ok(t, str) => Ok(StringV(t), str)
  | Error(stringE) =>
    switch StringSExp.parse(str, ~scope) {
    | Ok(t, str) => Ok(SExpV(t), str)
    | Error(sExpE) =>
      Error(
        `string or sexp expected.\nstring parsing failed with error: ${stringE}\nsexp parsing failed with error: ${sExpE}`,
      )
    }
  }
}

let prettyPrint = ((t, j): t, ~scope: array<StringTerm.meta>) =>
  `${StringTerm.prettyPrint(t, ~scope)} ${StringSExp.prettyPrint(j, ~scope)}`
let prettyPrintSubstCodom = (v: substCodom, ~scope: array<StringTerm.meta>) =>
  switch v {
  | StringV(t) => StringTerm.prettyPrint(t, ~scope)
  | SExpV(t) => StringSExp.prettyPrint(t, ~scope)
  }

let ghostJudgment = (StringTerm.ghostTerm, StringSExp.ghostTerm)
