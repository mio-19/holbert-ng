open Component

module TextArea = (Underlying : COMPONENT) => {
  module Ports = Underlying.Ports
  type props = { content: string, imports: Ports.t, onLoad: (~exports:Ports.t) => (), onChange: (string,~exports:Ports.t) => () }
  let make = (props) => {
    let (editing,setEditing) = React.useState (_ => false)
    let (text,setText) = React.useState (_ => props.content)
    let onTextChange= (ev: JsxEvent.Form.t) => {
      let target = JsxEvent.Form.target(ev)
      let value: string = target["value"]
      setText(_ => value)
    }
    let done = (_) => {
      setEditing(_=>false)
    }
    if editing {
      <div>
        <textarea className="editor-textArea" value={text} onChange={onTextChange} />
        <div className="editor-controls">
        <span className="editor-button button-icon button-icon-blue typcn typcn-tick" onClick={done}>
        </span>
        </div>
      </div>
    } else {
      <div>
      <Underlying
        content={text}
        imports={props.imports}
        onLoad={props.onLoad}
        onChange={(string, ~exports) => {
          setText(_=>string)
          props.onChange(string,~exports)
        }} />
      <div className="editor-controls">
      <span className="editor-button button-icon button-icon-blue typcn typcn-edit" onClick={_ => setEditing(_ => true)}>
      </span>
      </div>
      </div>
    }
  }
}