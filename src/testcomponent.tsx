import * as ComponentGraph2 from './componentgraph'
import { make as SExpBaseView, RuleSExpTE, PM } from './Scratch.mjs'
import ReactDOM from 'react-dom/client';
import React from 'react';

export let ComponentGraph = ComponentGraph2
type Component = ComponentGraph2.Component 
export class ConfigComponent implements Component {
	data : string; 
	dependencyChanged : (id: string, comp: Component) => void;
	toString() {
		return this.data //prettyPrint(this.data,["a","b","c"])
	}
	constructor(str: string,_deps : Record<string,Component>, signal : (msg: any) => void, view? : HTMLElement)	 {
		view.innerHTML='<input type="radio" id="style_Gentzen" name="style" value="Gentzen">';
		view.innerHTML+='<input type="radio" id="style_Linear" name="style" value="Linear">';
		view.innerHTML+='<input type="radio" id="style_Hybrid" name="style" value="Hybrid">';
		(document.getElementById("style_" + str) as HTMLInputElement).checked = true;
		for (const i of ["Gentzen","Linear","Hybrid"]) {
			(document.getElementById("style_" + i) as HTMLInputElement).addEventListener("change",(e) =>{ this.data = (e.target as any).value;  signal("changed")})
		}
		this.data = str;
		this.dependencyChanged = (_depName, _comp) => { };
	}
	
}

export class TestComponent implements Component {
	data : any;
	config : string;
	dependencyChanged : (id: string, comp: Component) => void;
	root : ReactDOM.Root;
	toString() {
		return (RuleSExpTE.serialise({rules:this.data, style:this.config},this.data))
	}
	constructor(str : string, deps : Record<string,Component>, signal : (msg: any) => void, view? : HTMLElement) {
		for (const x in deps) {
			if (deps[x] instanceof ConfigComponent) {
				this.config = deps[x].data
			}
		}
		this.data = (RuleSExpTE.deserialise({rules:this.data, style:this.config},str))["_0"]
		console.log(this.config)
		if (view != null) {
			this.root = ReactDOM.createRoot(view);
			this.root.render(<SExpBaseView rules={this.data} style={this.config}
			onChange={ e => { this.data = e.rules; signal("changed") }} />)
		}
		this.dependencyChanged = (_depName, comp) => { 
			if (comp instanceof ConfigComponent) {
				this.config = comp.data
				console.log(this.config)
				this.root.render(<SExpBaseView rules={this.data} style={this.config} 
					onChange={ e => { this.data = e.rules; signal("changed") }} />)
			}
		};
	}
}


export class ProofComponent implements Component {
	data : any;
	dependencies : Record<string,any>;
	dependencyChanged : (id: string, comp: Component) => void;
	root : ReactDOM.Root;
	toString() {
		return ""
	}
	constructor(str : string, deps : Record<string,Component>, signal : (msg: any) => void, view? : HTMLElement) {
		console.log("FOO")
		for (const x in deps) {
			if (deps[x] instanceof TestComponent) {
				this.dependencies = deps[x].data
			}
		}
		var gen = {contents:0};
		this.data = PM.parse(str, [], gen);
		console.log(this.data)
		if (view != null) {
			//this.root = ReactDOM.createRoot(view);
			//this.root.render(<SExpBaseView rules={this.data} style={this.config}
		//	onChange={ e => { this.data = e.rules; signal("changed") }} />)
		}
		this.dependencyChanged = (_depName, comp) => { 
		};
	}
}


window.localStorage.clear()
ComponentGraph.setup({"hol-comp": TestComponent,"hol-config": ConfigComponent, "hol-proof":ProofComponent});
