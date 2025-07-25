open Signatures
open Test

let assertEqual = (a, b, ~message=?) => assertion((a, b) => a === b, a, b, ~message?)

module MakeTerm = (Term: TERM) => {
  let termEquivalent = (t1: Term.t, t2: Term.t, ~message=?) => {
    assertion(Term.equivalent, t1, t2, ~message?)
  }
  let testParse = (input: string, t2: Term.t, ~message=?) => {
    let res = Term.parse(input, ~scope=[], ~gen=Term.makeGen())
    // i'm sure this could be handled better, but it works
    switch res {
    | Ok(res) => {
        assertEqual(res->snd, "", ~message=input ++ " input consumed")
        let message = message->Option.getOr(input ++ " equals expectation")
        termEquivalent(res->fst, t2, ~message)
      }
    | Error(msg) => fail(~message="parse failed: " ++ msg, ())
    }
  }
}
