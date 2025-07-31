type rec t =
  | Symbol({name: string})
  | Var({idx: int})
  | Schematic({schematic: int})
  | Lam({name: string, body: t})
  | App({func: t, arg: t})

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Map.t<int, t>

type stripped = {
  func: t,
  args: array<t>,
}
let strip: t => stripped
// exposed for testing purposes
let singletonSubst: (int, t) => subst
