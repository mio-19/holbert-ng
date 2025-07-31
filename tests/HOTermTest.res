open Zora
open HOTerm

module Util = TestUtil.MakeTerm(HOTerm)

let testUnify = (t: Zora.t, at: string, bt: string, ~subst=?, ~msg=?) => {
  let gen = HOTerm.makeGen()
  let (a, _) = HOTerm.parse(at, ~scope=[], ~gen)->Result.getExn
  let (b, _) = HOTerm.parse(bt, ~scope=[], ~gen)->Result.getExn
  try {
    let res = HOTerm.unifyTerm(a, b, HOTerm.emptySubst, ~gen=Some(gen))
    switch subst {
    | None => t->ok(true, ~msg=msg->Option.getOr("unification succeeded"))
    | Some(subst) =>
      t->equal(res, subst, ~msg=msg->Option.getOr("unification succeeded with substitution"))
    }
  } catch {
  | HOTerm.UnifyFail(failed) =>
    t->fail(
      ~msg="unification failed: " ++
      TestUtil.stringifyExn(a) ++
      " and " ++
      TestUtil.stringifyExn(b) ++
      " with error: " ++
      failed,
    )
  }
}
zoraBlock("parse symbol", t => {
  t->block("single char", t => t->Util.testParse("x", Symbol({name: "x"})))
  t->block("multi char", t => t->Util.testParse("xyz", Symbol({name: "xyz"})))
})

zoraBlock("parse var", t => {
  t->block("single digit", t => t->Util.testParse("\\1", Var({idx: 1})))
  t->block("multi digit", t => t->Util.testParse("\\234", Var({idx: 234})))
})

zoraBlock("parse schematic", t => {
  t->block("empty allowed", t => t->Util.testParse("?1", Schematic({schematic: 1})))
})

zoraBlock("parse application", t => {
  t->block("multiple", t => {
    t->Util.testParse("(a b)", App({func: Symbol({name: "a"}), arg: Symbol({name: "b"})}))
  })
  t->block("multiple more", t => {
    t->Util.testParse(
      "(a b c)",
      App({
        func: App({func: Symbol({name: "a"}), arg: Symbol({name: "b"})}),
        arg: Symbol({name: "c"}),
      }),
    )
  })
  t->block("multiple var", t => {
    t->Util.testParse(
      "(a \\1 ?1)",
      App({
        func: App({func: Symbol({name: "a"}), arg: Var({idx: 1})}),
        arg: Schematic({schematic: 1}),
      }),
    )
  })
})

zoraBlock("parse lambda", t => {
  t->block("simple", t => {
    t->Util.testParse("(x . x)", Lam({name: "x", body: Var({idx: 0})}))
    t->Util.testParse("(x. x)", Lam({name: "x", body: Var({idx: 0})}))
  })
  t->block("with application", t => {
    t->Util.testParse(
      "(x . x x)",
      Lam({name: "x", body: App({func: Var({idx: 0}), arg: Var({idx: 0})})}),
    )
    t->Util.testParse(
      "(x. x x)",
      Lam({name: "x", body: App({func: Var({idx: 0}), arg: Var({idx: 0})})}),
    )
  })
  t->block("with application 2args", t => {
    t->Util.testParse(
      "(x . y. x y)",
      Lam({
        name: "x",
        body: Lam({name: "y", body: App({func: Var({idx: 1}), arg: Var({idx: 0})})}),
      }),
    )
    t->Util.testParse("(x. y. x)", Lam({name: "x", body: Lam({name: "y", body: Var({idx: 1})})}))
  })
})

zoraBlock("unify test", t => {
  t->block("symbols", t => {
    let x = "x"
    let y = "y"
    t->testUnify(x, x)
    t->testUnify(y, y)
    t->Util.testNotUnify(y, x)
    t->Util.testNotUnify(x, y)
  })
  t->block("applications", t => {
    let ab = "(a b)"
    let cd = "(c d)"
    t->testUnify(ab, ab)
    t->testUnify(cd, cd)
    t->Util.testNotUnify(ab, cd)
    t->Util.testNotUnify(cd, ab)
  })
})
