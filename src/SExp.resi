// NOTE: ideally we want something like:
// include module type of SExpFunc.Make(ConstSymbol)
// this doesn't work because reasons. can come back to revisit
// but i honestly suspect this is a rescript compiler bug.

module Symbol: SExpFunc.SYMBOL with type t = string
type rec t =
  | Symbol(Symbol.t)
  | Compound({subexps: array<t>})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
  | Ghost

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Map.t<int, t>
let mapTerms: (t, t => t) => t
