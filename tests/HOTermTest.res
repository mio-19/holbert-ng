open Zora
open HOTerm

module Util = TestUtil.MakeTerm(HOTerm)

zoraBlock("parse symbol", t => {
  t->block("single char", t => t->Util.testParse("x", Symbol({name: "x"})))
  t->block("multi char", t => t->Util.testParse("xyz", Symbol({name: "xyz"})))
})

zoraBlock("parse var", t => {
  t->block("single digit", t => t->Util.testParse("\\1", Var({idx: 1})))
  t->block("multi digit", t => t->Util.testParse("\\234", Var({idx: 234})))
})

zoraBlock("parse schematic", t => {
  t->block("empty allowed", t => t->Util.testParse("?1()", Schematic({schematic: 1, allowed: []})))
  t->block("one allowed", t =>
    t->Util.testParse("?1(\\1)", Schematic({schematic: 1, allowed: [1]}))
  )
  t->block("multiple allowed", t =>
    t->Util.testParse("?1(\\1 \\23 \\4)", Schematic({schematic: 1, allowed: [1, 23, 4]}))
  )
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
      "(a \\1 ?1())",
      App({
        func: App({func: Symbol({name: "a"}), arg: Var({idx: 1})}),
        arg: Schematic({schematic: 1, allowed: []}),
      }),
    )
  })
})

zoraBlock("unify test", t => {
  t->block("symbols", t => {
    let x = "x"
    let y = "y"
    t->Util.testUnify(x, x)
    t->Util.testUnify(y, y)
    t->Util.testNotUnify(y, x)
    t->Util.testNotUnify(x, y)
  })
  t->block("applications", t => {
    let ab = "(a b)"
    let cd = "(c d)"
    t->Util.testUnify(ab, ab)
    t->Util.testUnify(cd, cd)
    t->Util.testNotUnify(ab, cd)
    t->Util.testNotUnify(cd, ab)
  })
  t->block("flex-rigid", t => {
    let v0 = "\\0"
    let s0 = "?0(\\0)"
    // TODO: needs to make gen work; gen needs to have seen exisiting sementics; should use parse instead of writing parsed terms directly to get a good gen
    //t->Util.testUnify(v0, s0)
  })
})
