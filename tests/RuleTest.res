open Signatures
open Zora

module MakeTest = (Term: TERM, Judgment: JUDGMENT with module Term := Term) => {
  module RuleInst = Rule.Make(Term, Judgment)
  let testParseInner = (t: Zora.t, input: string, expect: RuleInst.t, ~scope=[], ~msg=?) => {
    let res = RuleInst.parseInner(input, ~scope, ~gen=Term.makeGen())
    switch res {
    | Ok(res) => {
        t->equal(res->Pair.second, "", ~msg=input ++ " input consumed")
        t->equal(res->Pair.first, expect, ~msg?)
      }
    | Error(msg) => t->fail(~msg="parse failed: " ++ msg)
    }
  }
  let testParseTopLevel = (
    t: Zora.t,
    input: string,
    expect: (RuleInst.t, string),
    ~scope=[],
    ~msg=?,
  ) => {
    let res = RuleInst.parseTopLevel(input, ~scope, ~gen=Term.makeGen())
    switch res {
    | Ok(res) => {
        t->equal(res->Pair.second, "", ~msg=input ++ " input consumed")
        t->equal(res->Pair.first, expect, ~msg?)
      }
    | Error(msg) => t->fail(~msg="parse failed: " ++ msg)
    }
  }
}

module Symbol = AtomDef.MakeAtomAndView(
  Symbolic.Atom,
  Symbolic.AtomView,
  AtomDef.NilAtomList,
  AtomDef.NilAtomListView,
)
module StringSymbol = AtomDef.MakeAtomAndView(
  StringA.Atom,
  StringA.AtomView,
  Symbol.Atom,
  Symbol.AtomView,
)

zoraBlock("string terms", t => {
  module StringSExp = SExp.Make(StringSymbol.Atom)
  let wrapString = (s): StringSExp.t => Atom(AtomDef.AnyValue(StringA.BaseAtom.Tag, s))
  let wrapSymbol = (s): StringSExp.t => Atom(AnyValue(Symbolic.BaseAtom.Tag, s))
  module T = MakeTest(StringSExp, StringSExp)
  t->T.testParseInner(
    `[s1. ("$s1" p) |- ("($s1)" p)]`,
    {
      vars: ["s1"],
      premises: [
        {
          vars: [],
          premises: [],
          conclusion: StringSExp.Compound({
            subexps: [wrapString([StringA.Var({idx: 0})]), wrapSymbol("p")],
          }),
        },
      ],
      conclusion: StringSExp.Compound({
        subexps: [
          wrapString([StringA.String("("), StringA.Var({idx: 0}), StringA.String(")")]),
          wrapSymbol("p"),
        ],
      }),
    },
  )
})

zoraBlock("string HOTerms", t => {
  module StringHOTerm = HOTerm.Make(StringSymbol.Atom)
  let wrapString = (s): StringHOTerm.t => Symbol({
    name: AtomDef.AnyValue(StringA.BaseAtom.Tag, s),
    constructor: false,
  })
  let wrapSymbol = (s): StringHOTerm.t => Symbol({
    name: AtomDef.AnyValue(Symbolic.BaseAtom.Tag, s),
    constructor: false,
  })
  let app = StringHOTerm.app
  module T = MakeTest(StringHOTerm, StringHOTerm)
  t->T.testParseInner(
    `[s1. ("$s1" p) |- ("($s1)" p)]`,
    {
      vars: ["s1"],
      premises: [
        {
          vars: [],
          premises: [],
          conclusion: app(wrapString([StringA.Var({idx: 0})]), [wrapSymbol("p")]),
        },
      ],
      conclusion: app(
        wrapString([StringA.String("("), StringA.Var({idx: 0}), StringA.String(")")]),
        [wrapSymbol("p")],
      ),
    },
  )
})
