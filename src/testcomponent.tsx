import * as ComponentGraph2 from './componentgraph'
import { make as SExpBaseView, Theorem,TheoremTE, RuleSExpTE } from './Scratch.mjs'
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
			onChange={ e => { this.data = e.rules; 
				signal("changed");
				return {"TAG":"Ok","_0":{}} }} />)
		}
		this.dependencyChanged = (_depName, comp) => { 
			if (comp instanceof ConfigComponent) {
				this.config = comp.data
				this.root.render(<SExpBaseView rules={this.data} style={this.config} 
					onChange={ e => { 
						this.data = e.rules; 
						signal("changed");
						return {"TAG":"Ok","_0":{}} }} />)
			}
		};
	}
}


export class ProofComponent implements Component {
	data : any;
	deps : Record<string,Component>;
	dependencyChanged : (id: string, comp: Component) => void;
	root : ReactDOM.Root;
	gen : {contents: number}
  config : string;
	toString() {
		return TheoremTE.serialise({name:this.data.name,rule:this.data.rule,proof:this.data.proof},{name:this.data.name,rule:this.data.rule,proof:this.data.proof})
	}
	refreshData(str) {
		let dependencies = {}
		for (const x in this.deps) {
			if (this.deps[x] instanceof TestComponent) {
				for (let k in this.deps[x].data) {
					dependencies[k] = this.deps[x].data[k]
				}
			}
			if (this.deps[x] instanceof ConfigComponent) {
				this.config = this.deps[x].data
			}
		}
		this.data = {rule:undefined, name: "", proof:undefined}
		this.gen = {contents:0};
		this.data = (TheoremTE.deserialise({gen:this.gen,facts:dependencies},str))["_0"]
		if (this.root) {
			this.root.render(
				<Theorem 
					rule={this.data.rule} 
					proof={this.data.proof} 
					name={this.data.name} 
					style={this.config} 
					onChange={(e) => { 
						this.data.rule = e.rule;
						this.data.proof = e.proof;
						this.data.name = e.name;
						 return {"TAG":"Ok","_0":{}} }} 
					gen={this.gen} 
					facts={dependencies} />);
		}
		
	}
	constructor(str : string, deps : Record<string,Component>, signal : (msg: any) => void, view? : HTMLElement) {
		this.deps = deps;
		if (view != null) { this.root = ReactDOM.createRoot(view); }
		this.refreshData(str)
		this.dependencyChanged = (_depName, comp) => { 
			this.refreshData(this.toString()); signal("changed");
			console.log(this.data)
		};
		
	}
}


window.localStorage.clear()
ComponentGraph.setup({"hol-comp": TestComponent,"hol-config": ConfigComponent, "hol-proof":ProofComponent});
