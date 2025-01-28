"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[515],{15680:(e,n,a)=>{a.r(n),a.d(n,{MDXContext:()=>s,MDXProvider:()=>h,mdx:()=>x,useMDXComponents:()=>c,withMDXComponents:()=>m});var t=a(96540);function i(e,n,a){return n in e?Object.defineProperty(e,n,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[n]=a,e}function l(){return l=Object.assign||function(e){for(var n=1;n<arguments.length;n++){var a=arguments[n];for(var t in a)Object.prototype.hasOwnProperty.call(a,t)&&(e[t]=a[t])}return e},l.apply(this,arguments)}function r(e,n){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),a.push.apply(a,t)}return a}function o(e){for(var n=1;n<arguments.length;n++){var a=null!=arguments[n]?arguments[n]:{};n%2?r(Object(a),!0).forEach((function(n){i(e,n,a[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):r(Object(a)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(a,n))}))}return e}function d(e,n){if(null==e)return{};var a,t,i=function(e,n){if(null==e)return{};var a,t,i={},l=Object.keys(e);for(t=0;t<l.length;t++)a=l[t],n.indexOf(a)>=0||(i[a]=e[a]);return i}(e,n);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(t=0;t<l.length;t++)a=l[t],n.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(i[a]=e[a])}return i}var s=t.createContext({}),m=function(e){return function(n){var a=c(n.components);return t.createElement(e,l({},n,{components:a}))}},c=function(e){var n=t.useContext(s),a=n;return e&&(a="function"==typeof e?e(n):o(o({},n),e)),a},h=function(e){var n=c(e.components);return t.createElement(s.Provider,{value:n},e.children)},p="mdxType",u={inlineCode:"code",wrapper:function(e){var n=e.children;return t.createElement(t.Fragment,{},n)}},f=t.forwardRef((function(e,n){var a=e.components,i=e.mdxType,l=e.originalType,r=e.parentName,s=d(e,["components","mdxType","originalType","parentName"]),m=c(a),h=i,p=m["".concat(r,".").concat(h)]||m[h]||u[h]||l;return a?t.createElement(p,o(o({ref:n},s),{},{components:a})):t.createElement(p,o({ref:n},s))}));function x(e,n){var a=arguments,i=n&&n.mdxType;if("string"==typeof e||i){var l=a.length,r=new Array(l);r[0]=f;var o={};for(var d in n)hasOwnProperty.call(n,d)&&(o[d]=n[d]);o.originalType=e,o[p]="string"==typeof e?e:i,r[1]=o;for(var s=2;s<l;s++)r[s]=a[s];return t.createElement.apply(null,r)}return t.createElement.apply(null,a)}f.displayName="MDXCreateElement"},281:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>d,contentTitle:()=>r,default:()=>u,frontMatter:()=>l,metadata:()=>o,toc:()=>s});var t=a(58168),i=(a(96540),a(15680));const l={id:"changing",title:"How do I change a schema?",sidebar_label:"Changing a schema"},r=void 0,o={unversionedId:"schema/changing",id:"schema/changing",title:"How do I change a schema?",description:"Glean supports modifying the schema directly, while providing",source:"@site/docs/schema/changing.md",sourceDirName:"schema",slug:"/schema/changing",permalink:"/docs/schema/changing",draft:!1,editUrl:"https://github.com/facebookincubator/Glean/tree/main/glean/website/docs/schema/changing.md",tags:[],version:"current",frontMatter:{id:"changing",title:"How do I change a schema?",sidebar_label:"Changing a schema"},sidebar:"someSidebar",previous:{title:"Recursion",permalink:"/docs/schema/recursion"},next:{title:'The special "all" schema',permalink:"/docs/schema/all"}},d={},s=[{value:"Basic rules",id:"basic-rules",level:3},{value:"Compatibility",id:"compatibility",level:3},{value:"Default values",id:"default-values",level:3},{value:"What if my schema changes are incompatible?",id:"what-if-my-schema-changes-are-incompatible",level:3},{value:"How does it work?",id:"how-does-it-work",level:3},{value:"Evolving schemas",id:"evolving-schemas",level:3}],m=(c="FbInternalOnly",function(e){return console.warn("Component "+c+" was not imported, exported, or provided by MDXProvider as global scope"),(0,i.mdx)("div",e)});var c;const h={toc:s},p="wrapper";function u(e){let{components:n,...a}=e;return(0,i.mdx)(p,(0,t.A)({},h,a,{components:n,mdxType:"MDXLayout"}),(0,i.mdx)("p",null,"Glean supports modifying the schema directly, while providing\nbackwards compatibility between existing clients and data across the\nschema change."),(0,i.mdx)("h3",{id:"basic-rules"},"Basic rules"),(0,i.mdx)("p",null,"To preserve compatibility between clients and data, the schema can\nonly be changed in ",(0,i.mdx)("strong",{parentName:"p"},"compatible")," ways. This means:"),(0,i.mdx)("ul",null,(0,i.mdx)("li",{parentName:"ul"},"Adding or removing a field from a record, if the field has a ",(0,i.mdx)("strong",{parentName:"li"},"defaultable")," type (see ",(0,i.mdx)("a",{parentName:"li",href:"#default-values"},"Default values"),")"),(0,i.mdx)("li",{parentName:"ul"},"Adding or removing an alternative from a sum type"),(0,i.mdx)("li",{parentName:"ul"},"Adding or removing a predicate or type declaration"),(0,i.mdx)("li",{parentName:"ul"},"Changing a list to a set or vice versa")),(0,i.mdx)("p",null,"An example of an ",(0,i.mdx)("em",{parentName:"p"},"incompatible")," change would be changing the type of a\nfield, for example from ",(0,i.mdx)("inlineCode",{parentName:"p"},"nat")," to ",(0,i.mdx)("inlineCode",{parentName:"p"},"bool"),"."),(0,i.mdx)("p",null,"Of course it's fine to make arbitrary changes to a schema that you're\nworking on; the compatibility rules only come into effect when the\nschema is landed. From that point there might be existing clients and\ndatabases in use, so Glean will reject any incompatible schema changes."),(0,i.mdx)(m,{mdxType:"FbInternalOnly"},"The compatibility check shows up in CI as `sync-cf-main-diff` and is performed on any diff that changes a schema. It checks that the new schema is compatible with the existing schema and all previous versions of the schema still in use, so it will catch things like removing a field and then re-adding it with a different type."),(0,i.mdx)("h3",{id:"compatibility"},"Compatibility"),(0,i.mdx)("p",null,"When the schema is changed, Glean supports any combination of old or\nnew clients (that is, clients built against the old or new version of\nthe schema) with old or new data."),(0,i.mdx)("ul",null,(0,i.mdx)("li",{parentName:"ul"},"If the client requests a field that doesn't exist in the data, the field will be filled with its default value."),(0,i.mdx)("li",{parentName:"ul"},"If a query matches on an alternative that doesn't exist in the schema, the match will fail."),(0,i.mdx)("li",{parentName:"ul"},"If the data contains an alternative that doesn't exist in the client's schema, then the client will receive an ",(0,i.mdx)("inlineCode",{parentName:"li"},"unknown")," value (this is represented as an ",(0,i.mdx)("inlineCode",{parentName:"li"},"EMPTY")," constructor in the Thrift-generated datatypes).")),(0,i.mdx)("h3",{id:"default-values"},"Default values"),(0,i.mdx)("p",null,"A ",(0,i.mdx)("strong",{parentName:"p"},"defaultable")," type is any type that is not a predicate. So for\nexample, if ",(0,i.mdx)("inlineCode",{parentName:"p"},"P")," is a predicate type, then we could add a field ",(0,i.mdx)("inlineCode",{parentName:"p"},"f :\nmaybe P")," but not a field ",(0,i.mdx)("inlineCode",{parentName:"p"},"f : P"),". The reason for this restriction is\nthat Glean must be able to substitute a default value for the\nfield when a client is using the new schema but the data is using the\nold schema, and there cannot be a default value for a predicate."),(0,i.mdx)("p",null,"Default values for missing fields are determined according to the\nfollowing table:"),(0,i.mdx)("table",null,(0,i.mdx)("thead",{parentName:"table"},(0,i.mdx)("tr",{parentName:"thead"},(0,i.mdx)("th",{parentName:"tr",align:null},"Type"),(0,i.mdx)("th",{parentName:"tr",align:null},"Default value"))),(0,i.mdx)("tbody",{parentName:"table"},(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"nat")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"0"))),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"byte")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"0"))),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"string")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,'""'))),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"[T]")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"[]"))),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null," set T")),(0,i.mdx)("td",{parentName:"tr",align:null},"the empty set")),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"{ field\u2081 : T\u2081, ..., field\u2099 : T\u2099 }")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"{ field\u2081 = default(T\u2081), ..., field\u2099 = default(T\u2099) }"))),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"{ field\u2081 : T\u2081 ","|"," ... ","|"," field\u2099 : T\u2099 }")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"{ field\u2081 = default(T\u2081) }"))),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"bool")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"false"))),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"maybe T")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"nothing"))),(0,i.mdx)("tr",{parentName:"tbody"},(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"enum { name\u2081 ","|"," ... ","|"," name\u2099 }")),(0,i.mdx)("td",{parentName:"tr",align:null},(0,i.mdx)("code",null,"name\u2081"))))),(0,i.mdx)("h3",{id:"what-if-my-schema-changes-are-incompatible"},"What if my schema changes are incompatible?"),(0,i.mdx)("p",null,"If you don't care about backwards compatibility, then an easy\nworkaround is to just bump the version of the whole schema, so for\nexample if you have"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre"},"schema graphql.1 {\n...\n}\n")),(0,i.mdx)("p",null,"then change it to"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre"},"schema graphql.2 {\n...\n}\n")),(0,i.mdx)("p",null,"and make whatever changes you need. The new version of the schema is\nentirely separate from the old version as far as Glean is concerned,\nso there are no restrictions on what changes can be made."),(0,i.mdx)("p",null,"If you want to retain some backwards compatibility, then you can add\nnew predicates representing the new data. You have the option of"),(0,i.mdx)("ol",null,(0,i.mdx)("li",{parentName:"ol"},"Writing both the old and the new data to the database"),(0,i.mdx)("li",{parentName:"ol"},"Producing two databases, one with the old data and one with the new data. The databases can be distinguished by different names or different properties.")),(0,i.mdx)("p",null,"With approach (1), you can migrate clients incrementally and then eventually\nstop producing the old data. But the downside of this approach is that\nthe database may contain a lot more data than necessary; writing may\ntake a lot longer, and so on."),(0,i.mdx)("p",null,"With approach (2), you first have to ensure clients select the old\ndatabase. Then as you migrate clients, change them to use the new data\nand select the new database at the same time."),(0,i.mdx)("h3",{id:"how-does-it-work"},"How does it work?"),(0,i.mdx)("p",null,"A particular instance of the schema is identified by a\n",(0,i.mdx)("inlineCode",{parentName:"p"},"SchemaId"),". This is a hash value computed from the full contents of\nthe schema. The ",(0,i.mdx)("inlineCode",{parentName:"p"},"SchemaId")," of the current schema is available through\nthe ",(0,i.mdx)("inlineCode",{parentName:"p"},"schema_id")," value exported by the ",(0,i.mdx)("inlineCode",{parentName:"p"},"builtin")," schema (the\n",(0,i.mdx)("inlineCode",{parentName:"p"},"Glean.Schema.Builtin.Types")," module in Haskell)."),(0,i.mdx)("p",null,"The server keeps track of multiple instances of the schema in a\n",(0,i.mdx)("strong",{parentName:"p"},"schema index"),". Each instance of the schema is identified by its\n",(0,i.mdx)("inlineCode",{parentName:"p"},"SchemaId"),". Each database also contains the schema that it was written\nwith. The schema index is manipulated using the ",(0,i.mdx)("inlineCode",{parentName:"p"},"gen-schema")," tool in\nthe Glean repository; to use a particular index we can use ",(0,i.mdx)("inlineCode",{parentName:"p"},"--schema\nindexfile:FILE")," or ",(0,i.mdx)("inlineCode",{parentName:"p"},"--schema indexconfig:CONFIG"),"."),(0,i.mdx)("p",null,"A client sends its ",(0,i.mdx)("inlineCode",{parentName:"p"},"SchemaId")," to the server in the ",(0,i.mdx)("inlineCode",{parentName:"p"},"schema_id")," field\nof the query. For Haskell clients this is done automatically: the\nclient passes its ",(0,i.mdx)("inlineCode",{parentName:"p"},"SchemaId")," when initialising the Glean client\nlibrary, and this ",(0,i.mdx)("inlineCode",{parentName:"p"},"SchemaId")," is passed on to the server for every\nquery."),(0,i.mdx)("p",null,"The server knows which schema the client is using, so it can translate\nthe data in the database into the client's schema automatically."),(0,i.mdx)("p",null,"When a database is created, the schema used by the database is chosen\nby the ",(0,i.mdx)("inlineCode",{parentName:"p"},"glean.schema_id")," property set by the client creating the\ndatabase. Again for Haskell clients this happens automatically."),(0,i.mdx)("h3",{id:"evolving-schemas"},"Evolving schemas"),(0,i.mdx)("p",null,"Glean also allows backwards-compatibility between co-existing schemas,\nwhich can be useful if you want to perform schema changes in a more\nexplicit way, or to rename schemas."),(0,i.mdx)("p",null,"The feature is enabled using a top-level directive"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre"},"schema my_schema.2 evolves my_schema.1\n")),(0,i.mdx)("p",null,"This declaration has the effect of treating queries for ",(0,i.mdx)("inlineCode",{parentName:"p"},"my_schema.1")," predicates as if they were for ",(0,i.mdx)("inlineCode",{parentName:"p"},"my_schema.2"),". That is the query results will be retrieved from the database in the shape of a ",(0,i.mdx)("inlineCode",{parentName:"p"},"my_schema.2")," fact and transformed into a fact of the equivalent ",(0,i.mdx)("inlineCode",{parentName:"p"},"my_schema.1")," predicate specified in the query."),(0,i.mdx)("p",null,"The new schema must contain all the predicates of the old schema, either with new versions or old versions, and their definitions must be backwards compatible. We can achieve this by copying the entire content of the old schema into the new one and modifying it there."),(0,i.mdx)("p",null,"Now what should Glean do when a client asks for a fact from an old schema?"),(0,i.mdx)("ul",null,(0,i.mdx)("li",{parentName:"ul"},"Answer with db facts from the old schema"),(0,i.mdx)("li",{parentName:"ul"},"Answer with db facts from the new schema transformed into the old ones.")),(0,i.mdx)("p",null,"If there are no facts of the old schema in in the database we will take option 2.\nIf the database has any fact at all of the old schema we choose option 1."),(0,i.mdx)("p",null,"That is, schema evolutions only take effect if there are no facts of the old schema in the database; it is ignored otherwise."),(0,i.mdx)("p",null,"As an example suppose we start with the following schemas:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre"},'schema src.1 {\n   predicate File {\n     path : string\n   }\n}\n\nschema os.1 {\n  import src.1\n\n  predicate Permissions {\n    file : File,\n    permissions : nat\n  }\n}\n\nschema info.1 {\n  import src.1\n\n  predicate IsTemporary {\n    file : File\n  } F where F = src.File { path = "/tmp".. }\n}\n')),(0,i.mdx)("p",null,"Now we want to make a backward-compatible change to ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.File")," and add an ",(0,i.mdx)("inlineCode",{parentName:"p"},"extension")," field. We could add this to the file:"),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre"},"schema src.2 {\n   predicate File {\n     path : string,\n     extension : string\n   }\n}\n\nschema src.2 evolves src.1\n")),(0,i.mdx)("p",null,"Now if the indexer is still producing only ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.1")," facts, all other predicates will work as before and queries for ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.File.2")," will return no results."),(0,i.mdx)("p",null,"Once the indexer is changed to produce only ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.2")," facts queries like ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.File.1 _")," will be fulfilled using data from the ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.2")," schema, converting the ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.File.2")," results to the shape of ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.File.1")," before returning to the client."),(0,i.mdx)("p",null,"This is also the case in the derivation query of ",(0,i.mdx)("inlineCode",{parentName:"p"},"info.IsTemporary"),". Although ",(0,i.mdx)("inlineCode",{parentName:"p"},"info")," imports ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.1"),", the query will be transformed to use ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.2")," facts."),(0,i.mdx)("p",null,"On the other hand, ",(0,i.mdx)("inlineCode",{parentName:"p"},"os.Permissions")," will be empty. This must be the case because its first field references a ",(0,i.mdx)("inlineCode",{parentName:"p"},"src.File.1")," fact, of which there is none in the database. For this predicate to continue being available we must evolve its schema as well."),(0,i.mdx)("pre",null,(0,i.mdx)("code",{parentName:"pre"},"schema os.2 {             # changed\n  import src.2            # changed\n\n  predicate Permissions {\n    file : File,\n    permissions : nat\n  }\n}\n\nschema os.2 evolves os.1    # changed\n")))}u.isMDXComponent=!0}}]);