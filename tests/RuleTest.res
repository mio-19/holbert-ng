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

zoraBlock("string terms", t => {
  module StringSymbol = AtomDef.MakeAtomAndView(
    Coercible.StringA,
    StringA.AtomView,
    Coercible.Symbolic,
    Symbolic.AtomView,
  )
  module StringSExp = SExp.Make(StringSymbol.Atom)
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
            subexps: [
              [StringA.Var({idx: 0})]->StringSymbol.Atom.Left->StringSExp.Atom,
              "p"->StringSymbol.Atom.Right->StringSExp.Atom,
            ],
          }),
        },
      ],
      conclusion: StringSExp.Compound({
        subexps: [
          [StringA.String("("), StringA.Var({idx: 0}), StringA.String(")")]
          ->StringSymbol.Atom.Left
          ->StringSExp.Atom,
          "p"->StringSymbol.Atom.Right->StringSExp.Atom,
        ],
      }),
    },
  )
})
