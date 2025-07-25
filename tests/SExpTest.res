open Zora
open SExp

module Util = TestUtil.MakeTerm(SExp)

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

zoraBlock("parse compound", t => {
  t->block("unit", t => t->Util.testParse("()", Compound({subexps: []})))
  t->block("single", t => t->Util.testParse("(a)", Compound({subexps: [Symbol({name: "a"})]})))
  t->block("multiple", t => {
    t->Util.testParse(
      "(a \\1 ?1())",
      Compound({
        subexps: [Symbol({name: "a"}), Var({idx: 1}), Schematic({schematic: 1, allowed: []})],
      }),
    )
  })
})
