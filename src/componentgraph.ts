
export interface Component {
	toString(): string;
	dependencyChanged(id: string, comp: Component, msg: any): void;
}

let toInitialise: Array<string> = [];

function fetchAsDocument(url: string): Promise<{ doc: Document; contentType: string }> {
	return new Promise((resolve, reject) => {
		const xhr = new XMLHttpRequest();
		xhr.open("GET", url);
		xhr.responseType = "document";
		xhr.onload = () => {
			if (xhr.responseXML) {
				resolve({ doc: xhr.responseXML, contentType: xhr.getResponseHeader("content-type") || "" });
			} else {
				reject(new Error(`Failed to parse document at ${url}`));
			}
		};
		xhr.onerror = () => reject(new Error(`Network error fetching ${url}`));
		xhr.send();
	});
}
let xsltDocPromise: Promise<Document> | null = null;
function getXsltDoc(): Promise<Document> {
  if (xsltDocPromise == null) {
    xsltDocPromise = fetchAsDocument("/default.xsl").then(({doc}) => doc);
  }
  return xsltDocPromise;
}

let docInFlight: Record<string, Promise<Document>> = {};

async function loadDoc(requestURI: string): Promise<Document> {
  if (requestURI in docInFlight) {
    return docInFlight[requestURI];
  }
  let promise = (async (): Promise<Document> => {
    const { doc, contentType } = await fetchAsDocument(requestURI);
    const isXml = /\bxml\b/i.test(contentType) && !/html/i.test(contentType);
    if (isXml) {
      const xsltDoc = await getXsltDoc();
      const xsltProcessor = new XSLTProcessor();
      xsltProcessor.importStylesheet(xsltDoc);
      return xsltProcessor.transformToDocument(doc);
    } else {
      return doc;
    }
  })();
  docInFlight[requestURI] = promise;
  return promise;
}

let inFlight: Record<string, Promise<Handler>> = {};

export async function load(url: string): Promise<Handler> {
  if (url in database) {
    return database[url];
  }
  if (url in inFlight) {
    return inFlight[url];
  }

  let promise = (async (): Promise<Handler> => {
    let requestURI = url.split('/').slice(0, -1).join("/");
    let htmldoc = await loadDoc(requestURI);

    for (let x of knownTags) {
      for (let y of htmldoc.querySelectorAll(x)) {
        window.customElements.upgrade(document.adoptNode(y));
      }
    }
    while (toInitialise.length) {
      database[toInitialise.shift() ?? ""]?.initialise();
    }
    if (!(url in database)) {
      throw "ERROR";
    }
    return database[url];
  })();

  inFlight[url] = promise;
  try {
    return await promise;
  } finally {
    delete inFlight[url];
  }
}

class Handler {
	url: string;
	component: Component | null;
	subscribers: Array<Handler>;
	deps: Record<string, Handler | null>;
	status: "loading" | "ready";

	addSubscriber(h: Handler) {
		this.subscribers.push(h);
		if (this.status == "ready") {
			h.dependencyReady(this.url)
		}
	}
	initialise: () => void;
	dependencyReady: (url: string) => void;
	notifySubscribers: (msg: any) => void;
	constructor(
		url: string,
		textual: string,
		original_str: string,
		deps: Array<string>,
		maker: new (
			data: string,
			deps: Record<string, Component>,
			signal: (msg: any) => void,
			initialised: (msg: any) => void,
			reset: (msg: any) => void,
			view?: HTMLElement) => Component,
		view?: HTMLElement) {
		this.url = url;
		this.status = "loading";
		this.deps = {};
		let awaiting: Array<string> = [];
		this.subscribers = [];
		for (let dep of deps) {
			if (dep != "") {
				awaiting.push(dep);
				this.deps[dep] = null;
			}
		}
		this.component = null;
		this.notifySubscribers = function(msg: any) {
      console.log("NOTIFY", this.url, "subscriber count:", this.subscribers.length, this.subscribers.map(s => s.url))
 			if (this.component != null) {
				for (let sub of this.subscribers) {
					if (sub.component != null) {
						sub.component.dependencyChanged(this.url, this.component, msg)
					}
				}
				window.localStorage.setItem(this.url, this.component.toString())
			}
		}
		
		
		let setupComponent =  (text : string, deps: Record<string,Component>, notifyReady: boolean) => {
      console.log("Setting up ", text, deps)
			this.component = new maker(text, deps,
			(msg) => { this.notifySubscribers(msg) }, (msg) => {
				this.status = "ready";
        if (notifyReady) {
          queueMicrotask(() => {
            for (let sub of this.subscribers) {
					    sub.dependencyReady(this.url);
            } 
          })
        }
			}, (msg) => {
				setupComponent(original_str, deps, false);
				this.notifySubscribers(msg);
			}, view);
		}
		
		this.initialise = function() {
			for (let dep in this.deps) {
				load(dep).then((h) => {
					this.deps[dep] = h;
					h.addSubscriber(this);
				})
			}
			if (awaiting.length == 0) {
				setupComponent(textual, {}, true)
			}
		}
		this.dependencyReady = function(url) {
			let index = awaiting.indexOf(url);
			if (index > -1) {
				awaiting.splice(index, 1);
			}

			if (awaiting.length == 0) {
				let deps2: Record<string, Component> = {};
				for (let dep in this.deps) {
					let v = this.deps[dep]?.component;
					if (v != null && v != undefined) {
						deps2[dep] = v;
					}
				}				
				setupComponent(textual, deps2, true);
			}
		}
	}
}

let knownTags: Array<string> = [];
export let database: Record<string, Handler> = {};

export function setup(
	spec: Record<string,
		new (data: string,
			deps: Record<string, Component>,
			signal: (msg: any) => void,
			initialised: (msg: any) => void,
			reset: (msg: any) => void,
			view?: HTMLElement) => Component>) {
	let promises = [];
	for (const name in spec) {
		const maker = spec[name];
		knownTags.push(name);
		window.customElements.define(name, class extends HTMLElement {
			constructor() {
				super();
				let id = this.attributes.getNamedItem("id")?.value ?? "default";
				toInitialise.push(id);
				let deps = (this.attributes.getNamedItem("deps")?.value ?? "")
					.trim()
					.split(/\s+/)
					.filter(Boolean);
        deps = [...new Set(deps)];
				let original_str = this.textContent;
				let text = window.localStorage.getItem(id) ?? this.textContent;
				this.innerHTML = "loading";
				if (this.id in database) {
					this.innerHTML = "duplicate element"
				} else {
					database[this.id] = new Handler(this.id, text, original_str, deps, maker, this);
				}
			}
		});
		promises.push(window.customElements.whenDefined(name))
	}
	Promise.all(promises).then(() => {
		while (toInitialise.length) {
			database[toInitialise.shift() ?? ""]?.initialise();
		}
	})
}