type stringSExpAtom = StringS(StringAtom.t) | ConstS(SExp.Atom.t)

module StringSExpAtom: SExpFunc.ATOM with type t = stringSExpAtom = {
  type t = stringSExpAtom
  type subst = Map.t<int, t>
  type gen = ref<int>
  let parse = (s, ~scope, ~gen: option<gen>=?) => {
    StringAtom.parse(s, ~scope, ~gen?)
    ->Result.map(((r, rest)) => (StringS(r), rest))
    ->Util.Result.or(() =>
      SExp.Atom.parse(s, ~scope, ~gen?)->Result.map(((r, rest)) => (ConstS(r), rest))
    )
  }
  let prettyPrint = (s, ~scope) =>
    switch s {
    | StringS(s) => StringAtom.prettyPrint(s, ~scope)
    | ConstS(s) => SExp.Atom.prettyPrint(s, ~scope)
    }
  let unify = (s1, s2, ~gen=?) =>
    switch (s1, s2) {
    | (StringS(s1), StringS(s2)) =>
      StringAtom.unify(s1, s2, ~gen?)->Seq.map(subst => subst->Util.mapMapValues(v => StringS(v)))
    | (ConstS(s1), ConstS(s2)) =>
      SExp.Atom.unify(s1, s2, ~gen?)->Seq.map(subst => subst->Util.mapMapValues(v => ConstS(v)))
    | (_, _) => Seq.empty
    }
  let substitute = (s, subst: subst) =>
    switch s {
    | StringS(s) => {
        let stringSubs = subst->Util.mapMapValues(v =>
          switch v {
          | StringS(s) => s
          | _ => [StringAtom.Ghost]
          }
        )
        StringS(StringAtom.substitute(s, stringSubs))
      }
    | ConstS(s) => ConstS(s)
    }
  let upshift = (s, amount: int, ~from=?) =>
    switch s {
    | StringS(s) => StringS(s->StringAtom.upshift(amount, ~from?))
    | ConstS(s) => ConstS(s)
    }
  let lowerVar = idx => StringS([StringAtom.Var({idx: idx})])
  let lowerSchematic = (schematic, allowed) => StringS([StringAtom.Schematic({schematic, allowed})])
  let ghost = StringS([StringAtom.Ghost])
  let substDeBruijn = (s, substs: array<t>, ~from=?) =>
    switch s {
    | StringS(s) => {
        let stringSubs = substs->Array.map(v =>
          switch v {
          | StringS(s) => s
          | _ => [StringAtom.String("AYAYAYSLKDJFLSKDJ")]
          }
        )
        StringS(StringAtom.substDeBruijn(s, stringSubs, ~from?))
      }
    | ConstS(s) => ConstS(s)
    }
  let unifiesWithAnything = s =>
    switch s {
    | StringS(s) => StringAtom.unifiesWithAnything(s)
    | ConstS(_) => false
    }
}

include SExpFunc.Make(StringSExpAtom)
