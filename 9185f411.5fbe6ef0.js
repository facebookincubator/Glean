(window.webpackJsonp=window.webpackJsonp||[]).push([[28],{103:function(e,t,n){"use strict";n.r(t),n.d(t,"frontMatter",(function(){return c})),n.d(t,"metadata",(function(){return s})),n.d(t,"toc",(function(){return d})),n.d(t,"default",(function(){return l}));var a=n(3),r=n(7),i=(n(0),n(128)),o=["components"],c={id:"changing",title:"How do I change a schema?",sidebar_label:"Changing a schema"},s={unversionedId:"schema/changing",id:"schema/changing",isDocsHomePage:!1,title:"How do I change a schema?",description:"Predicates are never modified. We can only make new versions of a",source:"@site/../docs/schema/changing.md",slug:"/schema/changing",permalink:"/docs/schema/changing",editUrl:"https://www.internalfb.com/intern/diffusion/FBS/browse/master/fbcode/glean/website/../docs/schema/changing.md",version:"current",sidebar_label:"Changing a schema",sidebar:"someSidebar",previous:{title:"Recursion",permalink:"/docs/schema/recursion"},next:{title:"Workflow",permalink:"/docs/schema/workflow"}},d=[{value:"Adding new predicates",id:"adding-new-predicates",children:[]},{value:"Deleting predicates",id:"deleting-predicates",children:[]}],p={toc:d};function l(e){var t=e.components,n=Object(r.a)(e,o);return Object(i.b)("wrapper",Object(a.a)({},p,n,{components:t,mdxType:"MDXLayout"}),Object(i.b)("div",{className:"admonition admonition-important alert alert--info"},Object(i.b)("div",{parentName:"div",className:"admonition-heading"},Object(i.b)("h5",{parentName:"div"},Object(i.b)("span",{parentName:"h5",className:"admonition-icon"},Object(i.b)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"14",height:"16",viewBox:"0 0 14 16"},Object(i.b)("path",{parentName:"svg",fillRule:"evenodd",d:"M7 2.3c3.14 0 5.7 2.56 5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 0 1 1.3 8c0-3.14 2.56-5.7 5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7-3.14-7-7-7zm1 3H6v5h2V4zm0 6H6v2h2v-2z"}))),"important")),Object(i.b)("div",{parentName:"div",className:"admonition-content"},Object(i.b)("p",{parentName:"div"},"Predicates are never modified. We can only make new versions of a\npredicate, or delete an old version of a predicate when we no longer\nneed to read or write data using it."))),Object(i.b)("p",null,"You can only add new predicates or new versions of predicates, and\ndelete old ones. This is to ensure compatibilty between different\nversions of clients and databases: adding new predicates to the schema\ndoesn't break existing clients or indexers."),Object(i.b)("p",null,"To be specific, ",Object(i.b)("em",{parentName:"p"},"modifying")," a predicate means changing its type in any way. To modify a predicate you need to:"),Object(i.b)("ul",null,Object(i.b)("li",{parentName:"ul"},"Add a new version of the predicate, creating a new schema version at the same time if necessary.",Object(i.b)("ul",{parentName:"li"},Object(i.b)("li",{parentName:"ul"},"This may entail adding new versions of other predicates too, because predicates that depended on the old version of the predicate must now be copied so that they can point to the new predicate you created."))),Object(i.b)("li",{parentName:"ul"},"Update and recompile clients and indexers as necessary to use your new version. Most of the time we don't use explicit versions in client code, so usually updating a client is just a recompile after the schema update.")),Object(i.b)("p",null,"Changing the schema can present a tricky migration problem: there are indexers generating the data, clients reading the data, and existing databases that can contain either the old schema or the new schema. Glean provides features to make smooth migration possible, see ",Object(i.b)("a",{parentName:"p",href:"../derived#derived-predicates-for-schema-migration"},"Derived Predicates for Schema Migration")),Object(i.b)("div",{className:"admonition admonition-note alert alert--secondary"},Object(i.b)("div",{parentName:"div",className:"admonition-heading"},Object(i.b)("h5",{parentName:"div"},Object(i.b)("span",{parentName:"h5",className:"admonition-icon"},Object(i.b)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"14",height:"16",viewBox:"0 0 14 16"},Object(i.b)("path",{parentName:"svg",fillRule:"evenodd",d:"M6.3 5.69a.942.942 0 0 1-.28-.7c0-.28.09-.52.28-.7.19-.18.42-.28.7-.28.28 0 .52.09.7.28.18.19.28.42.28.7 0 .28-.09.52-.28.7a1 1 0 0 1-.7.3c-.28 0-.52-.11-.7-.3zM8 7.99c-.02-.25-.11-.48-.31-.69-.2-.19-.42-.3-.69-.31H6c-.27.02-.48.13-.69.31-.2.2-.3.44-.31.69h1v3c.02.27.11.5.31.69.2.2.42.31.69.31h1c.27 0 .48-.11.69-.31.2-.19.3-.42.31-.69H8V7.98v.01zM7 2.3c-3.14 0-5.7 2.54-5.7 5.68 0 3.14 2.56 5.7 5.7 5.7s5.7-2.55 5.7-5.7c0-3.15-2.56-5.69-5.7-5.69v.01zM7 .98c3.86 0 7 3.14 7 7s-3.14 7-7 7-7-3.12-7-7 3.14-7 7-7z"}))),"note")),Object(i.b)("div",{parentName:"div",className:"admonition-content"},Object(i.b)("p",{parentName:"div"},"if you're just changing the derivation of a derived predicate, there's no need to create a new predicate version. The new derivation will take effect, for both old and new databases, as soon as the schema change is deployed."))),Object(i.b)("h3",{id:"adding-new-predicates"},"Adding new predicates"),Object(i.b)("p",null,"If you're just adding new predicates or types, then you don't need to add a new schema version."),Object(i.b)("h3",{id:"deleting-predicates"},"Deleting predicates"),Object(i.b)("p",null,"In most cases it's safe to delete predicates from the schema, provided you have no existing client code using them."))}l.isMDXComponent=!0},128:function(e,t,n){"use strict";n.d(t,"a",(function(){return l})),n.d(t,"b",(function(){return h}));var a=n(0),r=n.n(a);function i(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function c(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){i(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)n=i[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var d=r.a.createContext({}),p=function(e){var t=r.a.useContext(d),n=t;return e&&(n="function"==typeof e?e(t):c(c({},t),e)),n},l=function(e){var t=p(e.components);return r.a.createElement(d.Provider,{value:t},e.children)},m={inlineCode:"code",wrapper:function(e){var t=e.children;return r.a.createElement(r.a.Fragment,{},t)}},b=r.a.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,o=e.parentName,d=s(e,["components","mdxType","originalType","parentName"]),l=p(n),b=a,h=l["".concat(o,".").concat(b)]||l[b]||m[b]||i;return n?r.a.createElement(h,c(c({ref:t},d),{},{components:n})):r.a.createElement(h,c({ref:t},d))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,o=new Array(i);o[0]=b;var c={};for(var s in t)hasOwnProperty.call(t,s)&&(c[s]=t[s]);c.originalType=e,c.mdxType="string"==typeof e?e:a,o[1]=c;for(var d=2;d<i;d++)o[d]=n[d];return r.a.createElement.apply(null,o)}return r.a.createElement.apply(null,n)}b.displayName="MDXCreateElement"}}]);