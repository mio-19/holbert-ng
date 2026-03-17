type stringAtom = StringS(StringTerm.t) | ConstS(SExp.Atom.t)

module StringAtom: SExpFunc.ATOM with type t = stringAtom = {
  type t = stringAtom
  type subst = Map.t<int, t>
  type gen = ref<int>
  let parse = (s, ~scope, ~gen: option<gen>=?) => {
    StringTerm.parse(s, ~scope, ~gen?)
    ->Result.map(((r, rest)) => (StringS(r), rest))
    ->Util.Result.or(() =>
      SExp.Atom.parse(s, ~scope, ~gen?)->Result.map(((r, rest)) => (ConstS(r), rest))
    )
  }
  let prettyPrint = (s, ~scope) =>
    switch s {
    | StringS(s) => StringTerm.prettyPrint(s, ~scope)
    | ConstS(s) => SExp.Atom.prettyPrint(s, ~scope)
    }
  let unify = (s1, s2) =>
    switch (s1, s2) {
    | (StringS(s1), StringS(s2)) =>
      StringTerm.unify(s1, s2)->Seq.map(subst => subst->Util.mapMapValues(v => StringS(v)))
    | (ConstS(s1), ConstS(s2)) =>
      SExp.Atom.unify(s1, s2)->Seq.map(subst => subst->Util.mapMapValues(v => ConstS(v)))
    | (_, _) => Seq.empty
    }
  let substitute = (s, subst: subst) =>
    switch s {
    | StringS(s) => {
        let stringSubs = subst->Util.mapMapValues(v =>
          switch v {
          | StringS(s) => s
          | _ => [StringTerm.Ghost]
          }
        )
        StringS(StringTerm.substitute(s, stringSubs))
      }
    | ConstS(s) => ConstS(s)
    }
  let upshift = (s, amount: int, ~from=?) =>
    switch s {
    | StringS(s) => StringS(s->StringTerm.upshift(amount, ~from?))
    | ConstS(s) => ConstS(s)
    }
  let lowerVar = idx => StringS([StringTerm.Var({idx: idx})])
  let lowerSchematic = (schematic, allowed) => StringS([StringTerm.Schematic({schematic, allowed})])
  let ghost = StringS([StringTerm.Ghost])
  let substDeBruijn = (s, substs: array<t>, ~from) =>
    switch s {
    | StringS(s) => {
        let stringSubs = substs->Array.map(v =>
          switch v {
          | StringS(s) => s
          | _ => [StringTerm.String("AYAYAYSLKDJFLSKDJ")]
          }
        )
        StringS(StringTerm.substDeBruijn(s, stringSubs, ~from))
      }
    | ConstS(s) => ConstS(s)
    }
  let unifiesWithAnything = s =>
    switch s {
    | StringS(s) => StringTerm.unifiesWithAnything(s)
    | ConstS(_) => false
    }
}

include SExpFunc.Make(StringAtom)
