open Signatures
open Zora

let stringifyExn = (t: 'a) => JSON.stringifyAny(t, ~space=2)->Option.getExn

module MakeTerm = (Term: TERM) => {
  let termEquivalent = (t: Zora.t, t1: Term.t, t2: Term.t, ~msg=?) => {
    t->ok(
      Term.equivalent(t1, t2),
      ~msg=msg->Option.getOr(`${stringifyExn(t1)} equivalent to ${stringifyExn(t2)}`),
    )
  }
  let testParse = (t: Zora.t, input: string, t2: Term.t, ~scope=[], ~msg=?) => {
    let res = Term.parse(input, ~scope, ~gen=Term.makeGen())
    switch res {
    | Ok(res) => {
        t->equal(res->snd, "", ~msg=input ++ " input consumed")
        // NOTE: we're checking for equality here, not equivalency
        // error messages are better this way
        t->equal(res->fst, t2, ~msg?)
      }
    | Error(msg) => t->fail(~msg="parse failed: " ++ msg)
    }
  }
  let testParseFail = (t: Zora.t, input: string, ~scope=[], ~msg=?) => {
    let res = Term.parse(input, ~scope, ~gen=Term.makeGen())
    switch res {
    | Ok((p, remaining)) =>
      t->fail(
        ~msg=`parse intended to fail, but succeeded: ${Term.prettyPrint(
            p,
            ~scope,
          )}\nremaining: ${remaining}`,
      )
    | Error(_) => t->ok(true)
    }
  }
}
