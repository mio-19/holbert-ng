open Zora
open StringTerm

module Util = TestUtil.MakeTerm(StringTerm)

zoraBlock("parse", t => {
  t->block("empty", t => t->Util.testParse(``, []))
  t->block("string literal", t => {
    t->Util.testParse(`""`, [String("")])
    t->Util.testParse(`"x"`, [String("x")])
    t->Util.testParse(`"xyz. 123"`, [String("xyz. 123")])
  })
  t->block("variables", t => {
    t->Util.testParse("\\1", [Var({idx: 1})])
    t->Util.testParse("\\10", [Var({idx: 10})])
    t->Util.testParse(`x`, ~scope=["x"], [Var({idx: 0})])
    t->Util.testParse("\\?1()", [Schematic({schematic: 1, allowed: []})])
    t->Util.testParseFail("\\?1")
    t->Util.testParse("\\?10()", [Schematic({schematic: 10, allowed: []})])
    t->Util.testParse("\\?1(1 23 4)", [Schematic({schematic: 1, allowed: [1, 23, 4]})])
  })
  t->block("flat concat", t => {
    t->Util.testParse(`"x"."y"`, [String("x"), String("y")])
    t->Util.testParse(
      `"x".\\?1(1 2 3).\\1.y`,
      ~scope=["y"],
      [String("x"), Schematic({schematic: 1, allowed: [1, 2, 3]}), Var({idx: 1}), Var({idx: 0})],
    )
    // can't end in concat
    t->Util.testParseFail(`"x".`)
  })
  t->block("paren", t => {
    // empty
    t->Util.testParse(`()`, [])
    // balanced single
    t->Util.testParse(`("x")`, [String("x")])
    t->Util.testParse(`(("x"))`, [String("x")])
    // unbalanced single
    t->Util.testParseFail(`("x"))`)
    t->Util.testParseFail(`(("x")`)
    // multi
    t->Util.testParse(`"x".("y"."z")`, [String("x"), String("y"), String("z")])
    t->Util.testParse(`("x"."y")."z"`, [String("x"), String("y"), String("z")])
    // can't concat
    t->Util.testParseFail(`("x".)."y"`)
  })
})
