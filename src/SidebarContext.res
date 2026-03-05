type t = {
  sidebarRef: React.ref<Nullable.t<Dom.element>>,
}

let context = React.createContext({
  sidebarRef: {React.current: Nullable.null},
})
@react.component
let make = (~children, ~sidebarRef) => {
  let value = {sidebarRef: sidebarRef}
  React.createElement(React.Context.provider(context), {value, children})
}