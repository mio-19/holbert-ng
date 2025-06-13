let newline = "\n"
let mapMapValues = (m: Map.t<'a,'b>, f: 'b => 'c) => {
  let nu = Map.make();
  m->Map.forEachWithKey((v,k) => {
    nu->Map.set(k,f(v))
  })
  nu
}
