// type level stuff to enable well-typed coercions
type atomTag<_> = ..
type rec anyValue = AnyValue(atomTag<'a>, 'a): anyValue

// to allow circular coercions, we declare base types
// separately from relevant implementation
module type BASE_ATOM = {
  type t
  type atomTag<_> += Tag: atomTag<t>
  let wrap: t => anyValue
}

module MakeBaseAtom = (
  T: {
    type t
  },
): (BASE_ATOM with type t = T.t) => {
  type t = T.t
  type atomTag<_> += Tag: atomTag<t>
  let wrap = t => AnyValue(Tag, t)
}

module type ATOM = {
  module BaseAtom: BASE_ATOM
  type t = BaseAtom.t
  type subst = Map.t<int, t>
  let unify: (t, t, ~gen: ref<int>=?) => Seq.t<subst>
  let prettyPrint: (t, ~scope: array<string>) => string
  let parse: (string, ~scope: array<string>, ~gen: ref<int>=?) => result<(t, string), string>
  let substitute: (t, subst) => t
  let upshift: (t, int, ~from: int=?) => t
  let substDeBruijn: (t, array<option<t>>, ~from: int=?) => t
  let concrete: t => bool
  let coerce: anyValue => option<t>
}

type varBase = Var({idx: int}) | Schematic({schematic: int, allowed: array<int>})
module VarBase = MakeBaseAtom({
  type t = varBase
})

exception AtomExpected

module AtomListBase = MakeBaseAtom({
  type t = anyValue
})

module type ATOM_LIST = {
  module HeadBase: BASE_ATOM
  include ATOM with module BaseAtom = AtomListBase
  let onHead: (t, HeadBase.t => 'a) => option<'a>
}

module NilAtomList: ATOM_LIST = {
  module HeadBase = MakeBaseAtom({
    // empty
    type t = {.}
  })
  module BaseAtom = AtomListBase
  type t = BaseAtom.t
  type subst = Map.t<int, t>
  let parse = (_, ~scope as _, ~gen as _=?) => Error("expected atom")
  // ideally we could check that the tags
  // in each argument are the same before returning Seq.empty, otherwise throw
  // but building up a type-level witness to tag equality is not easy with the
  // extensible variant stuff
  let unify = (_, _, ~gen as _=?) => Seq.empty
  // this should probably throw too, but will be more
  // informative to have it appear wherever it's called from
  let prettyPrint = (_, ~scope as _) => "NIL (THIS IS AN ERROR!)"
  let onHead = (_, _) => throw(AtomExpected)
  let coerce = _ => throw(AtomExpected)
  let substitute = (_, _) => throw(AtomExpected)
  let upshift = (_, _, ~from as _=?) => throw(AtomExpected)
  let substDeBruijn = (_, _, ~from as _=?) => throw(AtomExpected)
  let concrete = _ => throw(AtomExpected)
}

module CombineAtom = (Head: ATOM, Tail: ATOM_LIST): (
  ATOM_LIST with module HeadBase = Head.BaseAtom
) => {
  module HeadBase = Head.BaseAtom
  module Tail = Tail
  module BaseAtom = AtomListBase
  type t = BaseAtom.t
  type subst = Map.t<int, t>
  type gen = ref<int>
  let getOrElse = Util.Option.getOrElse
  let coerce = v => Some(v)
  let onHead = (AnyValue(tag, val), f: Head.t => 'a): option<'a> =>
    switch tag {
    | Head.BaseAtom.Tag => Some(f(val))
    | _ => None
    }
  let parse = (s, ~scope, ~gen: option<gen>=?) => {
    Head.parse(s, ~scope, ~gen?)
    ->Result.map(((r, rest)) => (HeadBase.wrap(r), rest))
    ->Util.Result.or(() => Tail.parse(s, ~scope, ~gen?))
  }
  let prettyPrint = (atom, ~scope) =>
    atom
    ->onHead(val => Head.prettyPrint(val, ~scope))
    ->getOrElse(() => Tail.prettyPrint(atom, ~scope))

  let unify = (a1, a2, ~gen=?) => {
    let (AnyValue(tag1, val1), AnyValue(tag2, val2)) = (a1, a2)
    switch (tag1, tag2) {
    | (Head.BaseAtom.Tag, Head.BaseAtom.Tag) =>
      Head.unify(val1, val2)->Seq.map(subst => subst->Util.mapMapValues(HeadBase.wrap))
    | (_, _) => Tail.unify(a1, a2, ~gen?)
    }
  }
  let coerceToHead = (atom): option<Head.t> =>
    atom->onHead(val => Some(val))->getOrElse(() => Head.coerce(atom))
  let substitute = (atom, subst: subst) =>
    atom
    ->onHead(val => {
      let leftSubs = subst->Util.Map.filterMap((_, v) => coerceToHead(v))
      Head.substitute(val, leftSubs)->HeadBase.wrap
    })
    ->getOrElse(() => Tail.substitute(atom, subst))

  let upshift = (atom, amount: int, ~from=?) =>
    atom
    ->onHead(val => Head.upshift(val, amount, ~from?)->HeadBase.wrap)
    ->getOrElse(() => Tail.upshift(atom, amount, ~from?))
  let substDeBruijn = (atom, substs: array<option<t>>, ~from=?) =>
    atom
    ->onHead(val =>
      Head.substDeBruijn(
        val,
        substs->Array.map(o => o->Option.flatMap(coerceToHead)),
        ~from?,
      )->HeadBase.wrap
    )
    ->getOrElse(() => Tail.substDeBruijn(atom, substs, ~from?))
  let concrete = atom => atom->onHead(Head.concrete)->getOrElse(() => Tail.concrete(atom))
}

module type ATOM_VIEW = {
  module Atom: ATOM
  type props = {name: Atom.t, scope: array<string>}
  let make: props => React.element
}

module NilAtomListView: ATOM_VIEW with module Atom := NilAtomList = {
  type props = {name: NilAtomList.t, scope: array<string>}
  let make = _ => throw(AtomExpected)
}

module MakeAtomView = (
  Left: ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: ATOM_LIST,
  RightView: ATOM_VIEW with module Atom := Right,
  Combined: module type of CombineAtom(Left, Right),
): (ATOM_VIEW with module Atom := Combined) => {
  type props = {name: Combined.t, scope: array<string>}
  let make = ({name, scope}: props) =>
    name
    ->Combined.onHead(left => <LeftView name={left} scope />)
    ->Util.Option.getOrElse(() => <RightView name scope />)
}

module MakeAtomAndView = (
  Left: ATOM,
  LeftView: ATOM_VIEW with module Atom := Left,
  Right: ATOM_LIST,
  RightView: ATOM_VIEW with module Atom := Right,
) => {
  module Atom = CombineAtom(Left, Right)
  module AtomView = MakeAtomView(Left, LeftView, Right, RightView, Atom)
}
