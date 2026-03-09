module ConstSymbol: SExpFunc.SYMBOL with type t = string = {
  type t = string
  type subst = Map.t<int, string>
  let unify = (a, b) =>
    if a == b {
      Seq.once(Map.make())
    } else {
      Seq.empty
    }
  let prettyPrint = (name, ~scope as _: array<string>) => name
  let parse = (string, ~scope as _: array<string>, ~gen as _=?) => Ok((string, ""))
  let substitute = (name, _) => name
  let constSymbol = name => Some(name)
}

include SExpFunc.Make(ConstSymbol)
let pSymbol = s => s->ConstSymbol.parse(~scope=[])->Result.getExn->Pair.first
let symbol = s => s->pSymbol->Symbol
let getSymbol = s => s->ConstSymbol.prettyPrint(~scope=[])
