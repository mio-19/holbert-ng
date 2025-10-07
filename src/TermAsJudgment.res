open Signatures

module Make = (Term: TERM): (JUDGMENT with module Term = Term and type t = Term.t) => {
  include Term
  module Term = Term
  type substCodom = Term.t
  let prettyPrintSubstCodom = Term.prettyPrint
  let parseSubstCodom = Term.parse
  let placeSubstCodom = Term.place
  let upshiftSubstCodom = Term.upshift
  let substituteSubstCodom = Term.substitute
}

module SExpJ = Make(SExp)
module HOTermJ = Make(HOTerm)
