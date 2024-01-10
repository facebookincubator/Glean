"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[4468],{3905:(e,n,a)=>{a.r(n),a.d(n,{MDXContext:()=>o,MDXProvider:()=>c,mdx:()=>N,useMDXComponents:()=>s,withMDXComponents:()=>p});var t=a(67294);function l(e,n,a){return n in e?Object.defineProperty(e,n,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[n]=a,e}function i(){return i=Object.assign||function(e){for(var n=1;n<arguments.length;n++){var a=arguments[n];for(var t in a)Object.prototype.hasOwnProperty.call(a,t)&&(e[t]=a[t])}return e},i.apply(this,arguments)}function d(e,n){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var t=Object.getOwnPropertySymbols(e);n&&(t=t.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),a.push.apply(a,t)}return a}function r(e){for(var n=1;n<arguments.length;n++){var a=null!=arguments[n]?arguments[n]:{};n%2?d(Object(a),!0).forEach((function(n){l(e,n,a[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):d(Object(a)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(a,n))}))}return e}function m(e,n){if(null==e)return{};var a,t,l=function(e,n){if(null==e)return{};var a,t,l={},i=Object.keys(e);for(t=0;t<i.length;t++)a=i[t],n.indexOf(a)>=0||(l[a]=e[a]);return l}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(t=0;t<i.length;t++)a=i[t],n.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(l[a]=e[a])}return l}var o=t.createContext({}),p=function(e){return function(n){var a=s(n.components);return t.createElement(e,i({},n,{components:a}))}},s=function(e){var n=t.useContext(o),a=n;return e&&(a="function"==typeof e?e(n):r(r({},n),e)),a},c=function(e){var n=s(e.components);return t.createElement(o.Provider,{value:n},e.children)},u={inlineCode:"code",wrapper:function(e){var n=e.children;return t.createElement(t.Fragment,{},n)}},x=t.forwardRef((function(e,n){var a=e.components,l=e.mdxType,i=e.originalType,d=e.parentName,o=m(e,["components","mdxType","originalType","parentName"]),p=s(a),c=l,x=p["".concat(d,".").concat(c)]||p[c]||u[c]||i;return a?t.createElement(x,r(r({ref:n},o),{},{components:a})):t.createElement(x,r({ref:n},o))}));function N(e,n){var a=arguments,l=n&&n.mdxType;if("string"==typeof e||l){var i=a.length,d=new Array(i);d[0]=x;var r={};for(var m in n)hasOwnProperty.call(n,m)&&(r[m]=n[m]);r.originalType=e,r.mdxType="string"==typeof e?e:l,d[1]=r;for(var o=2;o<i;o++)d[o]=a[o];return t.createElement.apply(null,d)}return t.createElement.apply(null,a)}x.displayName="MDXCreateElement"},57905:(e,n,a)=>{a.r(n),a.d(n,{assets:()=>m,contentTitle:()=>d,default:()=>s,frontMatter:()=>i,metadata:()=>r,toc:()=>o});var t=a(83117),l=(a(67294),a(3905));const i={id:"cli",title:"The Glean CLI tool",sidebar_label:"The Glean CLI tool"},d=void 0,r={unversionedId:"cli",id:"cli",title:"The Glean CLI tool",description:"The Glean CLI tool (glean) can create and write data into databases,",source:"@site/docs/cli.md",sourceDirName:".",slug:"/cli",permalink:"/docs/cli",draft:!1,editUrl:"https://github.com/facebookincubator/Glean/tree/main/glean/website/docs/cli.md",tags:[],version:"current",frontMatter:{id:"cli",title:"The Glean CLI tool",sidebar_label:"The Glean CLI tool"},sidebar:"someSidebar",previous:{title:"Running the Glean Server",permalink:"/docs/server"},next:{title:"Introduction",permalink:"/docs/indexer/intro"}},m={},o=[{value:"<code>glean create</code>",id:"glean-create",level:3},{value:"<code>glean write</code>",id:"glean-write",level:3},{value:"<code>glean finish</code>",id:"glean-finish",level:3},{value:"<code>glean dump</code>",id:"glean-dump",level:3},{value:"<code>glean delete</code>",id:"glean-delete",level:3},{value:"<code>glean derive</code>",id:"glean-derive",level:3},{value:"<code>glean index</code>",id:"glean-index",level:3},{value:"<code>glean query</code>",id:"glean-query",level:3},{value:"<code>glean restore</code>",id:"glean-restore",level:3},{value:"<code>glean validate</code>",id:"glean-validate",level:3},{value:"<code>glean validate-schema</code>",id:"glean-validate-schema",level:3},{value:"<code>glean stats</code>",id:"glean-stats",level:3},{value:"<code>glean unfinish</code>",id:"glean-unfinish",level:3}],p={toc:o};function s(e){let{components:n,...a}=e;return(0,l.mdx)("wrapper",(0,t.Z)({},p,a,{components:n,mdxType:"MDXLayout"}),(0,l.mdx)("p",null,"The Glean CLI tool (",(0,l.mdx)("inlineCode",{parentName:"p"},"glean"),") can create and write data into databases,\nperform a variety of admin tasks on databases, and also do one-off\nqueries."),(0,l.mdx)("p",null,"The ",(0,l.mdx)("inlineCode",{parentName:"p"},"glean")," tool accepts all the ",(0,l.mdx)("a",{parentName:"p",href:"/docs/running#common-options"},"common\noptions")," to specify how to connect to access\nthe databases."),(0,l.mdx)("admonition",{type:"note"},(0,l.mdx)("p",{parentName:"admonition"},"The ",(0,l.mdx)("inlineCode",{parentName:"p"},"--db")," flag used to be called ",(0,l.mdx)("inlineCode",{parentName:"p"},"--repo"),", ",(0,l.mdx)("inlineCode",{parentName:"p"},"--db-name")," used to be\n",(0,l.mdx)("inlineCode",{parentName:"p"},"--repo-name"),", and ",(0,l.mdx)("inlineCode",{parentName:"p"},"--db-instance")," used to be ",(0,l.mdx)("inlineCode",{parentName:"p"},"--repo-hash"),". The\nterminology was changed because databases don't necessarily correspond\nto repositories, and the instance doesn't necessarily correspond to a\nhash or revision of a repository.")),(0,l.mdx)("p",null,"The available commands are as follows:"),(0,l.mdx)("h3",{id:"glean-create"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean create")),(0,l.mdx)("p",null,"Create a new database."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-name NAME --db-instance INSTANCE"),(0,l.mdx)("br",null),"\nSpecifies the name and instance of the database"),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--finish"),(0,l.mdx)("br",null),"\nAlso mark the DB as complete"),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--stacked DB"),(0,l.mdx)("br",null),"\nCreate a stacked database on top of ",(0,l.mdx)("inlineCode",{parentName:"li"},"DB"),"."),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--property NAME=VALUE"),(0,l.mdx)("br",null),"\nSet properties when creating a DB"),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--update-schema-for-stacked"),(0,l.mdx)("br",null),"\nWhen creating a stacked DB, the schema is taken from the base DB. This\noption specifies that the current schema should be used instead. When\nusing this option, creation will fail if the current schema has\na different definition for any predicate in the base DB schema;\ntherefore predicates may only be added or removed relative to the base DB."),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"FILE.."),(0,l.mdx)("br",null),"\nFile(s) of facts to write into the database (JSON). See ",(0,l.mdx)("a",{parentName:"li",href:"/docs/write"},"Writing data\nto Glean"),".")),(0,l.mdx)("p",null,"The schema for the new DB is given by:"),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},"the property ",(0,l.mdx)("inlineCode",{parentName:"p"},"glean.schema_id")," if specified, or")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},"if ",(0,l.mdx)("inlineCode",{parentName:"p"},"--stacked")," or ",(0,l.mdx)("inlineCode",{parentName:"p"},"--incremental"),", then"),(0,l.mdx)("ul",{parentName:"li"},(0,l.mdx)("li",{parentName:"ul"},"if ",(0,l.mdx)("inlineCode",{parentName:"li"},"--update-schema-for-stacked")," is specified, then the default\nschema (or the one given by the ",(0,l.mdx)("inlineCode",{parentName:"li"},"--schema")," option),"),(0,l.mdx)("li",{parentName:"ul"},"otherwise, the schema from the base DB."))),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},"otherwise the default schema, or the one given by the ",(0,l.mdx)("inlineCode",{parentName:"p"},"--schema"),"\nflag."))),(0,l.mdx)("p",null,"Note that when creating a stacked DB, it is an error if the schema\ndoes not agree with the schema in the base DB for any predicate that\nhas facts. That is, you cannot change the schema in a stacked DB for\nexisting facts in the base DB."),(0,l.mdx)("h3",{id:"glean-write"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean write")),(0,l.mdx)("p",null,"Write facts to a database."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-name NAME --db-instance INSTANCE"),(0,l.mdx)("br",null),"\nSpecifies the name and instance of the database"),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"FILE.."),(0,l.mdx)("br",null),"\nFile(s) of facts to write into the database (JSON). See ",(0,l.mdx)("a",{parentName:"li",href:"/docs/write"},"Writing data\nto Glean"),"."),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--finish"),(0,l.mdx)("br",null),"\nAlso mark the DB as complete")),(0,l.mdx)("h3",{id:"glean-finish"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean finish")),(0,l.mdx)("p",null,"Notify server that a database is complete."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-name NAME --db-instance INSTANCE"),(0,l.mdx)("br",null),"\nSpecifies the name and instance of the database")),(0,l.mdx)("h3",{id:"glean-dump"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean dump")),(0,l.mdx)("p",null,"Dump the contents of the specified database into a file."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-name NAME --db-instance INSTANCE"),(0,l.mdx)("br",null),"\nSpecifies the name and instance of the database"),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"FILE"),(0,l.mdx)("br",null),"\nFile to write the facts into")),(0,l.mdx)("h3",{id:"glean-delete"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean delete")),(0,l.mdx)("p",null,"Delete a database."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-name NAME --db-instance INSTANCE"),(0,l.mdx)("br",null),"\nSpecifies the name and instance of the database")),(0,l.mdx)("h3",{id:"glean-derive"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean derive")),(0,l.mdx)("p",null,"Derive and store a predicate. See ",(0,l.mdx)("a",{parentName:"p",href:"/docs/derived"},"Derived Predicates"),"."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--page-bytes BYTES"),(0,l.mdx)("br",null),"\nMaximum number of bytes per page")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--page-facts FACTS"),(0,l.mdx)("br",null),"\nMaximum number of facts per page")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"PREDICATE"),(0,l.mdx)("br",null),"\nPredicates to derive"))),(0,l.mdx)("h3",{id:"glean-index"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean index")),(0,l.mdx)("p",null,"Index some source code using one of the known indexers."),(0,l.mdx)("p",null,"The form of the command in general is"),(0,l.mdx)("pre",null,(0,l.mdx)("code",{parentName:"pre"},"glean index LANGUAGE DIR --db NAME/INSTANCE\n")),(0,l.mdx)("p",null,"There may also be additional options accepted for each ",(0,l.mdx)("inlineCode",{parentName:"p"},"LANGUAGE"),"; try\n",(0,l.mdx)("inlineCode",{parentName:"p"},"glean index LANGUAGE --help")," to find out."),(0,l.mdx)("p",null,"For information on each indexer, see ",(0,l.mdx)("a",{parentName:"p",href:"/docs/indexer/intro"},"Indexers"),"."),(0,l.mdx)("h3",{id:"glean-query"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean query")),(0,l.mdx)("p",null,"Execute an Angle query and print the results, or write them to a file."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"p"},"--db-name NAME"),(0,l.mdx)("br",null),"\nSpecifies the database to query")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--page-bytes BYTES"),(0,l.mdx)("br",null),"\nMaximum number of bytes per page")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--page-facts FACTS"),(0,l.mdx)("br",null),"\nMaximum number of facts per page")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--recursive"),(0,l.mdx)("br",null),"\nFetch nested facts (slower)")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--limit FACTS"),(0,l.mdx)("br",null),"\nMaximum number of facts to query")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"-o,--output FILE"),(0,l.mdx)("br",null),"\nOutput the facts to a file")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--stats FILE"),(0,l.mdx)("br",null),"\nOutput stats to a file ('-' for stdout)")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--timeout MILLISECONDS"),"\nOverride the default query timeout")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"QUERY"),"\nquery to execute (",(0,l.mdx)("inlineCode",{parentName:"p"},"@file")," to read from file, ",(0,l.mdx)("inlineCode",{parentName:"p"},"-")," for stdin)")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--omit-results"),"\nDon't print results; use with ",(0,l.mdx)("inlineCode",{parentName:"p"},"--stat")," to get a count of results"))),(0,l.mdx)("h3",{id:"glean-restore"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean restore")),(0,l.mdx)("p",null,"Restore a database from backup."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"LOCATOR"),(0,l.mdx)("br",null),"\nDB location, see ",(0,l.mdx)("inlineCode",{parentName:"li"},":list-all")," in glean shell.")),(0,l.mdx)("p",null,"Alternatively the DB to restore can be specified by:"),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-name NAME")," and (",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-instance INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--date YYY-MM-DD"),")")),(0,l.mdx)("h3",{id:"glean-validate"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean validate")),(0,l.mdx)("p",null,"Perform checks on the internal integrity of a database. This is for\ntesting and debugging Glean itself."),(0,l.mdx)("p",null," a local database"),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"p"},"--db-name NAME --db-instance INSTANCE"),(0,l.mdx)("br",null),"\nSpecifies the name and instance of the database")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--no-typecheck"),(0,l.mdx)("br",null),"\nDon't typecheck facts.")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--no-keys"),(0,l.mdx)("br",null),"\nDon't verify key uniqueness")),(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("p",{parentName:"li"},(0,l.mdx)("inlineCode",{parentName:"p"},"--limit N"),(0,l.mdx)("br",null),"\nOnly validate the first N facts"))),(0,l.mdx)("h3",{id:"glean-validate-schema"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean validate-schema")),(0,l.mdx)("p",null,"Validate a schema. Checks that a new schem does not modify any of the\npredicates in the existing schema, which could lead to problems."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"FILE"),(0,l.mdx)("br",null),"\nName of schema file")),(0,l.mdx)("h3",{id:"glean-stats"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean stats")),(0,l.mdx)("p",null,"Get fact counts and sizes. Like the ",(0,l.mdx)("inlineCode",{parentName:"p"},":statistics")," command in the shell."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-name NAME --db-instance INSTANCE"),(0,l.mdx)("br",null),"\nSpecifies the name and instance of the database")),(0,l.mdx)("h3",{id:"glean-unfinish"},(0,l.mdx)("inlineCode",{parentName:"h3"},"glean unfinish")),(0,l.mdx)("p",null,"Unfinish a local database (turn it from complete to incomplete\nstate). This is for testing and development and not for routine use:\nonce a database is marked complete it could be replicated, so we\nshouldn't be modifying it."),(0,l.mdx)("ul",null,(0,l.mdx)("li",{parentName:"ul"},(0,l.mdx)("inlineCode",{parentName:"li"},"--db NAME/INSTANCE")," or ",(0,l.mdx)("inlineCode",{parentName:"li"},"--db-name NAME --db-instance INSTANCE"),(0,l.mdx)("br",null),"\nSpecifies the name and instance of the database")))}s.isMDXComponent=!0}}]);