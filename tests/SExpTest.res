open Test
open SExp

let intEqual = (~message=?, a: int, b: int) =>
  assertion(~message?, ~operator="Int equals", (a, b) => a === b, a, b)

module Util = TestUtil.MakeTerm(SExp)

test("parse symbol", () => {
  Util.testParse("x", Symbol({name: "x"}))
  Util.testParse("xyz", Symbol({name: "xyz"}))
})

test("parse var", () => {
  Util.testParse("\\1", Var({idx: 1}))
  Util.testParse("\\234", Var({idx: 234}))
})

test("parse schematic", () => {
  Util.testParse("?1()", Schematic({schematic: 1, allowed: []}))
  Util.testParse("?1(\\1)", Schematic({schematic: 1, allowed: [1]}))
  Util.testParse("?1(\\1 \\23 \\4)", Schematic({schematic: 1, allowed: [1, 23, 4]}))
})

test("parse compound", () => {
  Util.testParse("()", Compound({subexps: []}))
  Util.testParse("(a)", Compound({subexps: [Symbol({name: "a"})]}))
  Util.testParse(
    "(a \\1 ?1())",
    Compound({
      subexps: [Symbol({name: "a"}), Var({idx: 1}), Schematic({schematic: 1, allowed: []})],
    }),
  )
})
