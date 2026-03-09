module ConstSymbol: SExpFunc.SYMBOL = {
  type t = string
  type subst = Map.t<int, string>
  let unify = (a, b) =>
    if a == b {
      Seq.once(Map.make())
    } else {
      Seq.empty
    }
  let prettyPrint = (name, ~scope as _: array<string>) => name
  let parse = (string, ~scope as _: array<string>) => Ok((string, ""))
}

include SExpFunc.Make(ConstSymbol)
let symbol = s => s->ConstSymbol.parse(~scope=[])->Result.getExn->Pair.first->Symbol
let getSymbol = s => s->ConstSymbol.prettyPrint(~scope=[])
