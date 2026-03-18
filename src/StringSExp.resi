type stringSExpAtom = StringS(StringAtom.t) | ConstS(string)
module StringSExpAtom: SExpFunc.ATOM with type t = stringSExpAtom

include module type of SExpFunc.Make(StringSExpAtom)
