"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[5147],{15680:(e,n,t)=>{t.r(n),t.d(n,{MDXContext:()=>o,MDXProvider:()=>u,mdx:()=>N,useMDXComponents:()=>p,withMDXComponents:()=>s});var l=t(96540);function a(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(){return i=Object.assign||function(e){for(var n=1;n<arguments.length;n++){var t=arguments[n];for(var l in t)Object.prototype.hasOwnProperty.call(t,l)&&(e[l]=t[l])}return e},i.apply(this,arguments)}function r(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);n&&(l=l.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,l)}return t}function m(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?r(Object(t),!0).forEach((function(n){a(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):r(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function d(e,n){if(null==e)return{};var t,l,a=function(e,n){if(null==e)return{};var t,l,a={},i=Object.keys(e);for(l=0;l<i.length;l++)t=i[l],n.indexOf(t)>=0||(a[t]=e[t]);return a}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(l=0;l<i.length;l++)t=i[l],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(a[t]=e[t])}return a}var o=l.createContext({}),s=function(e){return function(n){var t=p(n.components);return l.createElement(e,i({},n,{components:t}))}},p=function(e){var n=l.useContext(o),t=n;return e&&(t="function"==typeof e?e(n):m(m({},n),e)),t},u=function(e){var n=p(e.components);return l.createElement(o.Provider,{value:n},e.children)},c="mdxType",x={inlineCode:"code",wrapper:function(e){var n=e.children;return l.createElement(l.Fragment,{},n)}},h=l.forwardRef((function(e,n){var t=e.components,a=e.mdxType,i=e.originalType,r=e.parentName,o=d(e,["components","mdxType","originalType","parentName"]),s=p(t),u=a,c=s["".concat(r,".").concat(u)]||s[u]||x[u]||i;return t?l.createElement(c,m(m({ref:n},o),{},{components:t})):l.createElement(c,m({ref:n},o))}));function N(e,n){var t=arguments,a=n&&n.mdxType;if("string"==typeof e||a){var i=t.length,r=new Array(i);r[0]=h;var m={};for(var d in n)hasOwnProperty.call(n,d)&&(m[d]=n[d]);m.originalType=e,m[c]="string"==typeof e?e:a,r[1]=m;for(var o=2;o<i;o++)r[o]=t[o];return l.createElement.apply(null,r)}return l.createElement.apply(null,t)}h.displayName="MDXCreateElement"},40476:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>d,contentTitle:()=>r,default:()=>h,frontMatter:()=>i,metadata:()=>m,toc:()=>o});var l=t(58168),a=(t(96540),t(15680));const i={id:"shell",title:"Using the Shell",sidebar_label:"Using the Shell"},r=void 0,m={unversionedId:"shell",id:"shell",title:"Using the Shell",description:"The shell is an interactive tool in which you can",source:"@site/docs/shell.md",sourceDirName:".",slug:"/shell",permalink:"/docs/shell",draft:!1,editUrl:"https://github.com/facebookincubator/Glean/tree/main/glean/website/docs/shell.md",tags:[],version:"current",frontMatter:{id:"shell",title:"Using the Shell",sidebar_label:"Using the Shell"},sidebar:"someSidebar",previous:{title:"Running the Tools",permalink:"/docs/running"},next:{title:"Running the Glean Server",permalink:"/docs/server"}},d={},o=[{value:"Invoking the shell",id:"invoking-the-shell",level:2},{value:"Quick start",id:"quick-start",level:2},{value:"Shell options",id:"shell-options",level:2}],s=e=>function(n){return console.warn("Component "+e+" was not imported, exported, or provided by MDXProvider as global scope"),(0,a.mdx)("div",n)},p=s("FbInternalOnly"),u=s("OssOnly"),c={toc:o},x="wrapper";function h(e){let{components:n,...t}=e;return(0,a.mdx)(x,(0,l.A)({},c,t,{components:n,mdxType:"MDXLayout"}),(0,a.mdx)("p",null,"The shell is an interactive tool in which you can"),(0,a.mdx)("ul",null,(0,a.mdx)("li",{parentName:"ul"},"Experiment with ",(0,a.mdx)("a",{parentName:"li",href:"/docs/angle/guide"},"Angle queries")),(0,a.mdx)("li",{parentName:"ul"},"Explore the data in a Glean database"),(0,a.mdx)("li",{parentName:"ul"},"Experiment with schema changes and ",(0,a.mdx)("a",{parentName:"li",href:"/docs/derived"},"derived predicates")),(0,a.mdx)("li",{parentName:"ul"},"Create experimental databases and query them")),(0,a.mdx)("h2",{id:"invoking-the-shell"},"Invoking the shell"),(0,a.mdx)(p,{mdxType:"FbInternalOnly"},(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre"},"glean shell\n")),(0,a.mdx)("p",null,"will start the shell, connect to the Glean query service and load up\nthe ",(0,a.mdx)("inlineCode",{parentName:"p"},"fbsource")," database.")),(0,a.mdx)(u,{mdxType:"OssOnly"},(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre"},"glean shell --service HOST:PORT\n")),(0,a.mdx)("p",null,"to connect to a server, or"),(0,a.mdx)("pre",null,(0,a.mdx)("code",{parentName:"pre"},"glean shell --db-root DIR\n")),(0,a.mdx)("p",null,"to use local databases from directory ",(0,a.mdx)("inlineCode",{parentName:"p"},"DIR"),".")),(0,a.mdx)("p",null,"See ",(0,a.mdx)("a",{parentName:"p",href:"#shell-options"},"shell options")," for more command-line options."),(0,a.mdx)("h2",{id:"quick-start"},"Quick start"),(0,a.mdx)("ul",null,(0,a.mdx)("li",{parentName:"ul"},"List the available databases with ",(0,a.mdx)("inlineCode",{parentName:"li"},":list")),(0,a.mdx)("li",{parentName:"ul"},"Select a database with ",(0,a.mdx)("inlineCode",{parentName:"li"},":db NAME")),(0,a.mdx)("li",{parentName:"ul"},"See the contents of the database with ",(0,a.mdx)("inlineCode",{parentName:"li"},":stat")),(0,a.mdx)("li",{parentName:"ul"},"Type queries in ",(0,a.mdx)("a",{parentName:"li",href:"/docs/angle/guide"},"Angle")," to see the results.")),(0,a.mdx)("h2",{id:"shell-options"},"Shell options"),(0,a.mdx)("p",null,"The shell accepts all the ",(0,a.mdx)("a",{parentName:"p",href:"/docs/running#common-options"},"common options"),". Additionally:"),(0,a.mdx)("ul",null,(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"QUERY")," or ",(0,a.mdx)("inlineCode",{parentName:"li"},":COMMAND"),(0,a.mdx)("br",null),"\nPerform the given ",(0,a.mdx)("inlineCode",{parentName:"li"},"QUERY")," or ",(0,a.mdx)("inlineCode",{parentName:"li"},":COMMAND")," and then exit. If multiple\ncommands or queries are given on the command line, they will be\nperformed in left-to-right order."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"--db NAME")," or ",(0,a.mdx)("inlineCode",{parentName:"li"},"--db NAME/HASH"),(0,a.mdx)("br",null),"\nLoad the database ",(0,a.mdx)("inlineCode",{parentName:"li"},"NAME")," or ",(0,a.mdx)("inlineCode",{parentName:"li"},"NAME/HASH"),"."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"--limit N"),(0,a.mdx)("br",null),"\nSet the default limit for queries; equivalent to the ",(0,a.mdx)("inlineCode",{parentName:"li"},":limit")," command."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"--width N"),(0,a.mdx)("br",null),"\nSet the terminal width for pretty-printing results."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"--pager"),(0,a.mdx)("br",null),"\nEnable automatic paging of results longer than a page."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"-v|--verbose N"),(0,a.mdx)("br",null),"\nEnabled verbosity at level ",(0,a.mdx)("inlineCode",{parentName:"li"},"N"))),(0,a.mdx)("h1",{id:"commands"},"Commands"),(0,a.mdx)("p",null,"Note that you can abbreviate commands as long as the abbreviation is\nunique. For example, ",(0,a.mdx)("inlineCode",{parentName:"p"},":edit")," can be abbreviated as ",(0,a.mdx)("inlineCode",{parentName:"p"},":e"),"."),(0,a.mdx)("ul",null,(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":database NAME")," or ",(0,a.mdx)("inlineCode",{parentName:"li"},":database NAME/HASH"),(0,a.mdx)("br",null),"\nUse database ",(0,a.mdx)("inlineCode",{parentName:"li"},"NAME")," or ",(0,a.mdx)("inlineCode",{parentName:"li"},"NAME/HASH"),"."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":list NAME"),(0,a.mdx)("br",null),"\nList available databases which match ",(0,a.mdx)("inlineCode",{parentName:"li"},"NAME"),"."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":list-all NAME"),(0,a.mdx)("br",null),"\nList available databases, and restorable backups, which match ",(0,a.mdx)("inlineCode",{parentName:"li"},"NAME"),"."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":index LANGUAGE DIR"),(0,a.mdx)("br",null),"\nIndex some source code for ",(0,a.mdx)("inlineCode",{parentName:"li"},"LANGUAGE")," in directory ",(0,a.mdx)("inlineCode",{parentName:"li"},"DIR"),", creating a\nnew database. This command is only available with the ",(0,a.mdx)("inlineCode",{parentName:"li"},"--db-root"),"\noption. Currently the only supported languages are ",(0,a.mdx)("inlineCode",{parentName:"li"},"flow")," and ",(0,a.mdx)("inlineCode",{parentName:"li"},"hack"),"."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":debug off|[-]ir|[-]bytecode|all"),(0,a.mdx)("br",null),"\nEnable query debugging; ",(0,a.mdx)("inlineCode",{parentName:"li"},":debug ir")," shows the intermediate\nrepresentation of the query after optimisation; ",(0,a.mdx)("inlineCode",{parentName:"li"},":debug\nbytecode")," shows the compiled bytecode."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":describe NAME"),(0,a.mdx)("br",null),"\nLike ",(0,a.mdx)("inlineCode",{parentName:"li"},":list"),", but show more details"),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":describe-all NAME"),(0,a.mdx)("br",null),"\nLike ",(0,a.mdx)("inlineCode",{parentName:"li"},":list-all"),", but show more details"),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":schema [PREDICATE|TYPE]"),(0,a.mdx)("br",null),"\nShow schema for the given ",(0,a.mdx)("inlineCode",{parentName:"li"},"PREDICATE")," or ",(0,a.mdx)("inlineCode",{parentName:"li"},"TYPE"),", or the whole schema\nif no predicate or type is given."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":use-schema [current|stored|<schema-id>]")," ",(0,a.mdx)("br",null),"\nWith no arguments, shows the available schemas and the current setting.\nWith an argument, selects the schema to use for queries and inspecting with the ",(0,a.mdx)("inlineCode",{parentName:"li"},":schema"),"\ncommand:",(0,a.mdx)("ul",{parentName:"li"},(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"current"),": selects the current schema."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"stored"),": selects the schema stored in the current DB."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},"<schema-id>"),": selects the specified schema."))),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":edit"),(0,a.mdx)("br",null),"\nEdit a query in an external editor. Set the ",(0,a.mdx)("inlineCode",{parentName:"li"},"EDITOR")," environment\nvariable to choose an editor."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":limit N"),(0,a.mdx)("br",null),"\nSet limit on the number of query results. If there are more results\nthan the limit, then you can type ",(0,a.mdx)("inlineCode",{parentName:"li"},":more")," to fetch the next ",(0,a.mdx)("inlineCode",{parentName:"li"},"N"),"\nresults."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":timeout off|MILLISECONDS"),(0,a.mdx)("br",null),"\nSet the query time budget. If the time limit expires, the results so\nfar are returned, and you can type ",(0,a.mdx)("inlineCode",{parentName:"li"},":more")," to see more results."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":expand off|on"),(0,a.mdx)("br",null),"\nRecursively expand nested facts in the response. ",(0,a.mdx)("inlineCode",{parentName:"li"},"on")," by default."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":pager off|on"),(0,a.mdx)("br",null),"\nEnable/disable result paging."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":count QUERY"),(0,a.mdx)("br",null),"\nShow only a count of query results, not the results themselves"),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":more"),(0,a.mdx)("br",null),"\nFetch more results from the previous query"),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":profile [off|summary|full]"),(0,a.mdx)("br",null),"\nShow query profiling information; see ",(0,a.mdx)("a",{parentName:"li",href:"/docs/angle/debugging"},"Query Debugging"),"."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":reload"),(0,a.mdx)("br",null),"\nReload the schema (when using ",(0,a.mdx)("inlineCode",{parentName:"li"},"--schema"),"). This command is useful when\nmaking changes to the schema, including ",(0,a.mdx)("a",{parentName:"li",href:"/docs/derived"},"derived\npredicates"),". Edit the schema source files, ",(0,a.mdx)("inlineCode",{parentName:"li"},":reload")," and then\ntest your changes."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":statistics [PREDICATE]"),(0,a.mdx)("br",null),"\nShow statistics for the current database."),(0,a.mdx)("li",{parentName:"ul"},(0,a.mdx)("inlineCode",{parentName:"li"},":quit"),(0,a.mdx)("br",null),"\nLeave the shell.")))}h.isMDXComponent=!0}}]);