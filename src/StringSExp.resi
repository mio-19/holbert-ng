type stringAtom = StringS(StringTerm.t) | ConstS(SExp.Atom.t)
module StringAtom: SExpFunc.ATOM with type t = stringAtom

type rec t =
  | Atom(StringAtom.t)
  | Compound({subexps: array<t>})
  | Var({idx: int})
  | Schematic({schematic: int, allowed: array<int>})
  | Ghost

include Signatures.TERM
  with type t := t
  and type meta = string
  and type schematic = int
  and type subst = Map.t<int, t>

module Atom: SExpFunc.ATOM with type t := StringAtom.t
let mapTerms: (t, t => t) => t
