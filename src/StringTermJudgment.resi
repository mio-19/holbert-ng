include Signatures.JUDGMENT with module Term := StringTerm
// TODO: is this okay? needed for destructuring in StringTermJView
let get: t => (StringTerm.t, string)
