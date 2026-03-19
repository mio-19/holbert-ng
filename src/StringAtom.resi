type rec piece =
  | String(string)
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
type t = array<piece>
type subst = Map.t<int, t>
type gen = ref<int>

include SExpFunc.ATOM with type t := t and type subst := subst
