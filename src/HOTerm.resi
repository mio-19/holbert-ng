open Util
type rec t =
  | Symbol({name: string})
  | Compound({subexps: array<t>})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
  | Lam({name: masked<string>, body: t})

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Map.t<int, t>
