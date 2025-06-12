## Holbert NG

This is a new, from-scratch development of my [reconceived design](https://liamoc.net/forest/loc-000V) of the Holbert proof assistant -- the browser based Higher Order Logic proof assistant.

Rather than a standalone web-app, Holbert NG will be a library of web components that can be seamlessly integrated into online documents. 
I envision it working like this, for HTML documents:

Index.html:
```html
<html>
  <head>
    <script src="holbert.mjs" type="module" />
    <script type="module">
      Holbert.setup({
        termLanguage: FirstOrderTerms, //also planned: HigherOrderTerms, Strings
        components: [GrammarDefinition, FunctionDefinition] // examples of extra components provided by planned plugins
        methods: [CalculationalProof, HoareDerivation] // examples of extra proof methods provided by planned plugins.
      })
    </script>
  </head>
  <body>
    <hol-config id="index.html/displaysettings">
       { rules: "Gentzen" }
    </hol-config>
    <p>Lets introduce the rules for conjunction:</p>
    <hol-rules id="index.html/andRules" deps="index.html/displaysettings">
      A.B.
        A B
        -------- andI
        (/\ A B)
      A.B.
        (/\ A B)
        -------- andE1
        A
      
      A.B.
        (/\ A B)
        -------- andE2
        B
    </hol-rules>
    <p>Proving commutativity of conjunction:</p>
    <hol-proof id="index.html/andComm" deps="index.html/displaysettings index.html/andRules">
      forall A B.
      assuming *: (/\ B A)
      shows (/\ A B)
      by andI(A B) {
      - by andE2(A B) {
        - by *
        }
      - by andE1(A B) {
        - by *
        }
      }
    </hol-proof>
  </body>
</html>
```

Note that the ids of each block are prefixed with a file name. These documents can span multiple HTML pages and any other dependencies are automatically fetched. 
Modifications are automatically saved to local storage.
In addition, the entire language of Holbert NG is representable both textually and graphically.

I plan to develop a small plug-in to make these components easily integratable into [Forester](https://www.forester-notes.org) documents as well.
