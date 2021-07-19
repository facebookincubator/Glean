(window.webpackJsonp=window.webpackJsonp||[]).push([[40],{115:function(e,t,a){"use strict";a.r(t),a.d(t,"frontMatter",(function(){return l})),a.d(t,"metadata",(function(){return d})),a.d(t,"toc",(function(){return s})),a.d(t,"default",(function(){return b}));var n=a(3),r=a(7),i=(a(0),a(128)),o=a(39),c=["components"],l={id:"derived",title:"Derived Predicates",sidebar_label:"Derived Predicates"},d={unversionedId:"derived",id:"derived",isDocsHomePage:!1,title:"Derived Predicates",description:'Glean supports predicates that are defined in terms of a query. There are two types of derived predicates, "stored" and "on demand".',source:"@site/../docs/derived.md",slug:"/derived",permalink:"/docs/derived",editUrl:"https://www.internalfb.com/intern/diffusion/FBS/browse/master/fbcode/glean/website/../docs/derived.md",version:"current",sidebar_label:"Derived Predicates",sidebar:"someSidebar",previous:{title:"Haskell Query API",permalink:"/docs/query/api/haskell"},next:{title:"Glean Databases",permalink:"/docs/databases"}},s=[{value:"Stored derived predicates",id:"stored-derived-predicates",children:[{value:"When do the facts get computed and stored?",id:"when-do-the-facts-get-computed-and-stored",children:[]},{value:"Deriving multiple predicates",id:"deriving-multiple-predicates",children:[]}]},{value:"On-demand derived predicates",id:"on-demand-derived-predicates",children:[]},{value:"Derived predicates for schema migration",id:"derived-predicates-for-schema-migration",children:[{value:"Default derived predicates",id:"default-derived-predicates",children:[]}]},{value:"How do I write and test a derived predicate?",id:"how-do-i-write-and-test-a-derived-predicate",children:[]},{value:"How do I make a derived predicate available?",id:"how-do-i-make-a-derived-predicate-available",children:[]}],p={toc:s};function b(e){var t=e.components,a=Object(r.a)(e,c);return Object(i.b)("wrapper",Object(n.a)({},p,a,{components:t,mdxType:"MDXLayout"}),Object(i.b)("p",null,'Glean supports predicates that are defined in terms of a query. There are two types of derived predicates, "stored" and "on demand".'),Object(i.b)("h2",{id:"stored-derived-predicates"},"Stored derived predicates"),Object(i.b)("p",null,"For example:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=angle"},"predicate OutTarget.1 :\n    {\n        file : src.File,\n        target : Target,\n    }\n    stored {F,T} where TargetOut {T,F}\n")),Object(i.b)("p",null,"This is a schema for a predicate ",Object(i.b)("inlineCode",{parentName:"p"},"OutTarget")," with a key type as usual. But unlike a regular predicate, facts of this predicate are not generated by an indexer, instead they are generated by the query given on the final line."),Object(i.b)("p",null,"The keyword ",Object(i.b)("inlineCode",{parentName:"p"},"stored")," tells Glean that the facts for this predicate will be stored in the database. Omitting the ",Object(i.b)("inlineCode",{parentName:"p"},"stored")," keyword indicates that you want the facts to be generated on demand; more about this in ",Object(i.b)("a",{parentName:"p",href:"#on-demand-derived-predicates"},"On-demand derived predicates")," below."),Object(i.b)("p",null,"You can read the query as"),Object(i.b)("blockquote",null,Object(i.b)("p",{parentName:"blockquote"},"There is a fact OutTarget {F,T} for every fact TargetOut {T,F}")),Object(i.b)("p",null,"The query can be any arbitrary Angle query; the syntax is described in\n",Object(i.b)("a",{parentName:"p",href:"angle/guide"},"Angle Guide"),". The only requirement is that the values\nproduced by the query must match the key type of the predicate being\ndefined."),Object(i.b)("p",null,"Why is this useful?  Well, the predicate ",Object(i.b)("inlineCode",{parentName:"p"},"TargetOut")," is defined like this:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=angle"},"predicate TargetOut.1 :\n    {\n        target : Target,\n        file : src.File,\n    }\n")),Object(i.b)("p",null,"This is a mapping from ",Object(i.b)("inlineCode",{parentName:"p"},"Target")," to ",Object(i.b)("inlineCode",{parentName:"p"},"File")," (see ",Object(i.b)("a",{parentName:"p",href:"angle/efficiency#efficient-matching-of-facts"},"Efficient matching of facts"),").  If we want the reverse mapping, from ",Object(i.b)("inlineCode",{parentName:"p"},"File")," to ",Object(i.b)("inlineCode",{parentName:"p"},"Target"),", we need a predicate with the fields in the other order, which is exactly what ",Object(i.b)("inlineCode",{parentName:"p"},"OutTarget")," is. But it would be laborious to write actual code to generate and store these facts in the database, so Glean allows us to define ",Object(i.b)("inlineCode",{parentName:"p"},"OutTarget")," directly in terms of a query, and it will automatically compute the facts of ",Object(i.b)("inlineCode",{parentName:"p"},"OutTarget")," and store them in the database."),Object(i.b)("h3",{id:"when-do-the-facts-get-computed-and-stored"},"When do the facts get computed and stored?"),Object(i.b)("p",null,"Using the ",Object(i.b)("inlineCode",{parentName:"p"},"glean")," command-line tool, you direct the server to compute and store the facts for a predicate like this:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=shell"},"glean --service <write-server> derive buck.TargetOut\n")),Object(i.b)("p",null,"Replacing ",Object(i.b)("inlineCode",{parentName:"p"},"<write-server>")," with the appropriate name of the write\nservice you're using, and replace ",Object(i.b)("inlineCode",{parentName:"p"},"buck.TargetOut")," with the name of\nthe predicate you want to derive."),Object(i.b)("p",null,"This may take some time, depending on how many facts need to be computed and stored."),Object(i.b)("div",{className:"admonition admonition-note alert alert--secondary"},Object(i.b)("div",{parentName:"div",className:"admonition-heading"},Object(i.b)("h5",{parentName:"div"},Object(i.b)("span",{parentName:"h5",className:"admonition-icon"},Object(i.b)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"14",height:"16",viewBox:"0 0 14 16"},Object(i.b)("path",{parentName:"svg",fillRule:"evenodd",d:"M6.3 5.69a.942.942 0 0 1-.28-.7c0-.28.09-.52.28-.7.19-.18.42-.28.7-.28.28 0 .52.09.7.28.18.19.28.42.28.7 0 .28-.09.52-.28.7a1 1 0 0 1-.7.3c-.28 0-.52-.11-.7-.3zM8 7.99c-.02-.25-.11-.48-.31-.69-.2-.19-.42-.3-.69-.31H6c-.27.02-.48.13-.69.31-.2.2-.3.44-.31.69h1v3c.02.27.11.5.31.69.2.2.42.31.69.31h1c.27 0 .48-.11.69-.31.2-.19.3-.42.31-.69H8V7.98v.01zM7 2.3c-3.14 0-5.7 2.54-5.7 5.68 0 3.14 2.56 5.7 5.7 5.7s5.7-2.55 5.7-5.7c0-3.15-2.56-5.69-5.7-5.69v.01zM7 .98c3.86 0 7 3.14 7 7s-3.14 7-7 7-7-3.12-7-7 3.14-7 7-7z"}))),"note")),Object(i.b)("div",{parentName:"div",className:"admonition-content"},Object(i.b)("p",{parentName:"div"},"Remember to do this ",Object(i.b)("em",{parentName:"p"},"before")," using ",Object(i.b)("inlineCode",{parentName:"p"},"glean finish")," to mark the database\nas finished."))),Object(i.b)("h3",{id:"deriving-multiple-predicates"},"Deriving multiple predicates"),Object(i.b)("p",null,"You can derive multiple predicates together:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=shell"},"glean --service <write-server> derive <predicate> <predicate> ...\n")),Object(i.b)("p",null,"But note that these predicates must be independent; they cannot depend\non each other. If you have derived predicates that depend on each\nother, you have to issue separate ",Object(i.b)("inlineCode",{parentName:"p"},"glean derive")," commands to derive\nthe predicates in bottom-up dependency order."),Object(i.b)("h2",{id:"on-demand-derived-predicates"},"On-demand derived predicates"),Object(i.b)("p",null,"The other type of derived predicate is one where the facts are not stored in the database, but are computed on-demand when there is a query for the predicate."),Object(i.b)("p",null,"This is useful for a few reasons:"),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"We can support ",Object(i.b)("a",{parentName:"li",href:"#derived-predicates-for-schema-migration"},"backwards compatibility")," by defining old predicates in terms of new ones, or forwards compatibility by doing the reverse."),Object(i.b)("li",{parentName:"ul"},"We can define queries that extract data and bundle it in a way that's convenient and efficient for the client. This allows clients to avoid fetching more data than they need, for example."),Object(i.b)("li",{parentName:"ul"},"Most importantly, we can encapsulate complex queries by defining them in the schema as derived predicates, even building up libraries representing whole abstraction layers over the raw data. Clients can then use the higher-level abstraction regardless of where they're querying from or what language they're using. For a great example of using this in practice, see the ",Object(i.b)("a",{parentName:"li",href:"https://github.com/facebookincubator/Glean/blob/master/glean/schema/source/codemarkup.angle"},"codemarkup schema")," that we use to provide a language-neutral abstraction over language-specific schemas.")),Object(i.b)("p",null,"For example, in the ",Object(i.b)("inlineCode",{parentName:"p"},"cxx")," schema we have a lot of different kinds of declarations. Clients often want to search for a declaration by name, but each of the different declaration kinds has the name in a different place, so this ends up being quite a complicated query from the client side. Using a derived predicate we can easily capture this complexity in one place so that it can be reused by all clients that want to search for declarations by name:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=angle"},"predicate DeclarationWithName :\n    {\n        name : string,\n        decl : Declaration\n    }\n    {Str, Decl} where\n      N = Name Str;\n      Decl =\n        (Declaration (record_ R) where\n          R = RecordDeclaration { name = { name = N }}) |\n        (Declaration (function_ F) where\n          F = FunctionDeclaration { name = { name = { name = N }}}) |\n        # and so on, for all declaration types\n")),Object(i.b)("p",null,"Using this predicate requires no magic on the part of the client, they just query for the ",Object(i.b)("inlineCode",{parentName:"p"},"cxx1.DeclarationWithName")," predicate in exactly the same way as they would for other predicates, and the Glean query server returns the appropriate facts."),Object(i.b)("h2",{id:"derived-predicates-for-schema-migration"},"Derived predicates for schema migration"),Object(i.b)("p",null,"One important use case for derived predicates is to make it possible to change the schema without breaking things."),Object(i.b)("p",null,"Essentially the idea is that a derived predicate can define the old predicate in terms of the new predicate, providing backwards-compatibility to clients that are expecting to query for the old predicate. Additionally, we might define the new predicate in terms of the old predicate, for forwards-compatibility to allow new clients to work with old data."),Object(i.b)("p",null,"Let's work through an example to illustrate the process.  Suppose your schema is like this:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=angle"},"schema lang.1 {\n\npredicate Declaration :\n    {\n         name : string,\n         source : src.Range,\n     }\n}\n")),Object(i.b)("p",null,"now suppose we want to add documentation to the declarations that we indexed. We define a new version of the schema, ",Object(i.b)("inlineCode",{parentName:"p"},"lang.2"),", with a new ",Object(i.b)("inlineCode",{parentName:"p"},"Declaration")," predicate:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=angle"},"schema lang.2 : lang.1 {\n\npredicate Declaration :\n    {\n        name : string,\n        source : src.Range,\n        doc : string\n    }\n}\n")),Object(i.b)("p",null,"Now, we proceed to make our changes:"),Object(i.b)("ol",null,Object(i.b)("li",{parentName:"ol"},"Update the schema"),Object(i.b)("li",{parentName:"ol"},"Modify the indexer to generate facts of the new predicate ",Object(i.b)("inlineCode",{parentName:"li"},"lang.Declaration.2"))),Object(i.b)("p",null,"At this point, any DBs generated by the new indexer will have ",Object(i.b)("inlineCode",{parentName:"p"},"lang.Declaration.2")," facts, and not ",Object(i.b)("inlineCode",{parentName:"p"},"lang.Declaration.1"),". Existing clients that query for the old facts will get no results. We can probably recompile those clients to pick up the new ",Object(i.b)("inlineCode",{parentName:"p"},"lang.Declaration.2")," facts, but that would be a tricky migration: the new client won't work on the old DBs, and the old client won't work on the new DBs."),Object(i.b)("p",null,"To make this migration smoother, we can add a derived predicate:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=angle"},"schema lang.2 : lang.1 {\n\npredicate Declaration :\n    {\n        name : string,\n        source : src.Range,\n        doc : string\n    }\n\n derive lang.Declaration.1\n    { name = N, source = S } where\n        lang.Declaration.2 { name = N, source = S }\n}\n")),Object(i.b)("p",null,"the ",Object(i.b)("inlineCode",{parentName:"p"},"derive lang.Declaration.1")," declaration is just like adding an on-demand derived predicate to ",Object(i.b)("inlineCode",{parentName:"p"},"predicate Declaration")," in the ",Object(i.b)("inlineCode",{parentName:"p"},"lang.1")," schema, but we have to declare it as part of the ",Object(i.b)("inlineCode",{parentName:"p"},"lang.2")," schema because it needs to refer to ",Object(i.b)("inlineCode",{parentName:"p"},"lang.Declaration.2"),"."),Object(i.b)("p",null,"This derived predicate takes effect as follows:"),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"It ",Object(i.b)("strong",{parentName:"li"},"does not apply to old DBs")," that contain ",Object(i.b)("inlineCode",{parentName:"li"},"lang.Declaration.1")," but not ",Object(i.b)("inlineCode",{parentName:"li"},"lang.Declaration.2"),"."),Object(i.b)("li",{parentName:"ul"},"It ",Object(i.b)("strong",{parentName:"li"},"does apply to new DBs")," created with the new ",Object(i.b)("inlineCode",{parentName:"li"},"lang.Declaration.2")," schema. So after the schema change, we can only create facts of ",Object(i.b)("inlineCode",{parentName:"li"},"lang.Declaration.2"),", not the old predicate.")),Object(i.b)("p",null,"So clients that query for ",Object(i.b)("inlineCode",{parentName:"p"},"lang.Declaration.1")," will continue to work both with old DBs containing ",Object(i.b)("inlineCode",{parentName:"p"},"lang.Declaration.1")," ",Object(i.b)("em",{parentName:"p"},"and")," new DBs that contain ",Object(i.b)("inlineCode",{parentName:"p"},"lang.Declaration.2"),", and we can migrate them to use the new schema at our leisure."),Object(i.b)("h3",{id:"default-derived-predicates"},"Default derived predicates"),Object(i.b)("p",null,"There's one extra feature that can be used to make the schema migration even smoother."),Object(i.b)("p",null,"Recall with the ",Object(i.b)("inlineCode",{parentName:"p"},"derive")," declaration in the previous section we had to synchronise the update of the schema with the rollout of the new version of the indexer to generate the new facts? It's possible to decouple those by making one tweak:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=angle"}," derive lang.Declaration.1 default\n    # ... same as before\n")),Object(i.b)("p",null,"The addition of the ",Object(i.b)("inlineCode",{parentName:"p"},"default")," keyword to the declaration has the following effect:"),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"A ",Object(i.b)("inlineCode",{parentName:"li"},"default")," derived predicate only takes effect when the DB is complete (i.e. read-only) and ",Object(i.b)("strong",{parentName:"li"},"contains no facts")," of the predicate.")),Object(i.b)("p",null,"This allows us to update the schema but still generate facts of the old predicate. The derived predicate will only kick in when we update the indexer to generate the new facts."),Object(i.b)("p",null,"What's more, we can use this technique to provide ",Object(i.b)("strong",{parentName:"p"},"forwards compatibility")," too:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=angle"},' derive lang.Declaration.2 default\n    { name = N, source = S, doc = "" } where\n        lang.Declaration.1 { name = N, source = S }\n')),Object(i.b)("p",null,"Since this is a ",Object(i.b)("inlineCode",{parentName:"p"},"default")," derivation, it will take effect when there are no facts of the new predicate. So we can update clients to work with the new version of the predicate, and they will continue to work on old DBs - albeit with empty strings for the new ",Object(i.b)("inlineCode",{parentName:"p"},"doc")," field, because the old DBs don't contain that data."),Object(i.b)("h2",{id:"how-do-i-write-and-test-a-derived-predicate"},"How do I write and test a derived predicate?"),Object(i.b)("p",null,"There's a process for iterating and testing derived predicates using the shell with a local database. Follow these steps:"),Object(i.b)(o.FbInternalOnly,{mdxType:"FbInternalOnly"},Object(i.b)("p",null,"Make a dir to store your local Glean DBs."),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=shell"},"mkdir ~/local/gleandb\n")),Object(i.b)("p",null,"Download the DB you want to test with:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=shell"},"glean --db-root ~/local/gleandb restore --repo-name fbsource --date 2021-04-29\n")),Object(i.b)("p",null,"(replace ",Object(i.b)("inlineCode",{parentName:"p"},"fbsource")," and the date as appropriate). This may take a while.")),Object(i.b)(o.OssOnly,{mdxType:"OssOnly"},Object(i.b)("p",null,"Obtain the DB you want to test with, let's assume you put it in\n",Object(i.b)("inlineCode",{parentName:"p"},"~/local/gleandb"),".")),Object(i.b)("p",null,"Start the shell with the local DB and schema:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=shell"},"glean-shell --db-root ~/local/gleandb --schema glean/schema/source\n")),Object(i.b)("p",null,"Add ",Object(i.b)("inlineCode",{parentName:"p"},"--db-schema-override")," if you are working on an existing predicate and want your version to override the schema in the DB."),Object(i.b)("p",null,"Select your DB with the ",Object(i.b)("inlineCode",{parentName:"p"},":db")," command."),Object(i.b)("p",null,"Make edits to the local schema source files in ",Object(i.b)("inlineCode",{parentName:"p"},"glean/schema/source"),". There's no need to run ",Object(i.b)("inlineCode",{parentName:"p"},"glean/schema/sync"),", you can pick up the changes immediately in the shell:"),Object(i.b)("pre",null,Object(i.b)("code",{parentName:"pre",className:"language-lang=shell"},":reload\n")),Object(i.b)("p",null,"Test your derived predicate using queries in the shell, use ",Object(i.b)("inlineCode",{parentName:"p"},":reload")," to pick up new changes, and repeat as necessary."),Object(i.b)("p",null,"The ",Object(i.b)("inlineCode",{parentName:"p"},":timeout")," command can be used to change the default query timeout while iterating."),Object(i.b)("p",null,"If you run into performance issues, try the techniques in ",Object(i.b)("a",{parentName:"p",href:"angle/debugging"},"Debugging\nQueries"),"."),Object(i.b)("p",null,"When you're done, the next section describes how to get your derived predicate into the schema proper."),Object(i.b)("h2",{id:"how-do-i-make-a-derived-predicate-available"},"How do I make a derived predicate available?"),Object(i.b)("p",null,"Derived predicates are defined directly in the schema, so the process\nfor adding them is exactly the same as modifying the schema, described\nover in ",Object(i.b)("a",{parentName:"p",href:"schema/workflow"},"Schema Workflow"),"."))}b.isMDXComponent=!0},128:function(e,t,a){"use strict";a.d(t,"a",(function(){return p})),a.d(t,"b",(function(){return u}));var n=a(0),r=a.n(n);function i(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function c(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){i(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function l(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},i=Object.keys(e);for(n=0;n<i.length;n++)a=i[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(n=0;n<i.length;n++)a=i[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var d=r.a.createContext({}),s=function(e){var t=r.a.useContext(d),a=t;return e&&(a="function"==typeof e?e(t):c(c({},t),e)),a},p=function(e){var t=s(e.components);return r.a.createElement(d.Provider,{value:t},e.children)},b={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},h=r.a.forwardRef((function(e,t){var a=e.components,n=e.mdxType,i=e.originalType,o=e.parentName,d=l(e,["components","mdxType","originalType","parentName"]),p=s(a),h=n,u=p["".concat(o,".").concat(h)]||p[h]||b[h]||i;return a?r.a.createElement(u,c(c({ref:t},d),{},{components:a})):r.a.createElement(u,c({ref:t},d))}));function u(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var i=a.length,o=new Array(i);o[0]=h;var c={};for(var l in t)hasOwnProperty.call(t,l)&&(c[l]=t[l]);c.originalType=e,c.mdxType="string"==typeof e?e:n,o[1]=c;for(var d=2;d<i;d++)o[d]=a[d];return r.a.createElement.apply(null,o)}return r.a.createElement.apply(null,a)}h.displayName="MDXCreateElement"}}]);