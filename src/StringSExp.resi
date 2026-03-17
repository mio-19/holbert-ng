type stringSExpAtom = StringS(StringAtom.t) | ConstS(SExp.Atom.t)
module StringSExpAtom: SExpFunc.ATOM with type t = stringSExpAtom

type rec t =
  | Atom(StringSExpAtom.t)
  | Compound({subexps: array<t>})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
  | Ghost

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Map.t<int, t>

module Atom: SExpFunc.ATOM with type t := StringSExpAtom.t
let mapTerms: (t, t => t) => t
