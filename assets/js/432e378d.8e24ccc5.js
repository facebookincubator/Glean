"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[9519],{3905:(e,n,t)=>{t.r(n),t.d(n,{MDXContext:()=>m,MDXProvider:()=>p,mdx:()=>x,useMDXComponents:()=>c,withMDXComponents:()=>u});var r=t(67294);function a(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(){return i=Object.assign||function(e){for(var n=1;n<arguments.length;n++){var t=arguments[n];for(var r in t)Object.prototype.hasOwnProperty.call(t,r)&&(e[r]=t[r])}return e},i.apply(this,arguments)}function d(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);n&&(r=r.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,r)}return t}function o(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?d(Object(t),!0).forEach((function(n){a(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):d(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function l(e,n){if(null==e)return{};var t,r,a=function(e,n){if(null==e)return{};var t,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||(a[t]=e[t]);return a}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(a[t]=e[t])}return a}var m=r.createContext({}),u=function(e){return function(n){var t=c(n.components);return r.createElement(e,i({},n,{components:t}))}},c=function(e){var n=r.useContext(m),t=n;return e&&(t="function"==typeof e?e(n):o(o({},n),e)),t},p=function(e){var n=c(e.components);return r.createElement(m.Provider,{value:n},e.children)},s={inlineCode:"code",wrapper:function(e){var n=e.children;return r.createElement(r.Fragment,{},n)}},f=r.forwardRef((function(e,n){var t=e.components,a=e.mdxType,i=e.originalType,d=e.parentName,m=l(e,["components","mdxType","originalType","parentName"]),u=c(t),p=a,f=u["".concat(d,".").concat(p)]||u[p]||s[p]||i;return t?r.createElement(f,o(o({ref:n},m),{},{components:t})):r.createElement(f,o({ref:n},m))}));function x(e,n){var t=arguments,a=n&&n.mdxType;if("string"==typeof e||a){var i=t.length,d=new Array(i);d[0]=f;var o={};for(var l in n)hasOwnProperty.call(n,l)&&(o[l]=n[l]);o.originalType=e,o.mdxType="string"==typeof e?e:a,d[1]=o;for(var m=2;m<i;m++)d[m]=t[m];return r.createElement.apply(null,d)}return r.createElement.apply(null,t)}f.displayName="MDXCreateElement"},16459:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>m,contentTitle:()=>o,default:()=>p,frontMatter:()=>d,metadata:()=>l,toc:()=>u});var r=t(83117),a=(t(67294),t(3905)),i=t(44256);const d={id:"thrift",title:"Thrift and JSON",sidebar_label:"Thrift and JSON"},o=void 0,l={unversionedId:"schema/thrift",id:"schema/thrift",title:"Thrift and JSON",description:"The Glean schema is automatically translated into a set of Thrift type",source:"@site/docs/schema/thrift.md",sourceDirName:"schema",slug:"/schema/thrift",permalink:"/docs/schema/thrift",draft:!1,editUrl:"https://github.com/facebookincubator/Glean/tree/main/glean/website/docs/schema/thrift.md",tags:[],version:"current",frontMatter:{id:"thrift",title:"Thrift and JSON",sidebar_label:"Thrift and JSON"},sidebar:"someSidebar",previous:{title:"Workflow",permalink:"/docs/schema/workflow"},next:{title:"Design",permalink:"/docs/schema/design"}},m={},u=[],c={toc:u};function p(e){let{components:n,...t}=e;return(0,a.mdx)("wrapper",(0,r.Z)({},c,t,{components:n,mdxType:"MDXLayout"}),(0,a.mdx)("p",null,"The Glean schema is automatically translated into a set of Thrift type\ndefinitions by the ",(0,a.mdx)("inlineCode",{parentName:"p"},"gen-schema")," tool (see ",(0,a.mdx)("a",{parentName:"p",href:"/docs/schema/workflow"},"Workflow"),").\nThese Thrift definitions can be used to work with Glean data in your\nclient, as native data types in whatever language you're using, either\nfor querying data or for writing facts."),(0,a.mdx)("p",null,"The Thrift types also have a JSON representation, which can be read\nand written directly. When you perform queries in the\n",(0,a.mdx)("a",{parentName:"p",href:"/docs/shell"},"shell"),", the results are printed as JSON-encoded Thrift;\nwhen you ",(0,a.mdx)("a",{parentName:"p",href:"/docs/write"},"write data to Glean")," it can be in the form of\nJSON-encoded Thrift."),(0,a.mdx)(i.FbInternalOnly,{mdxType:"FbInternalOnly"},(0,a.mdx)("p",null,"Facebook internal: the Thrift types for the schema are automatically\ngenerated into\n",(0,a.mdx)("a",{parentName:"p",href:"https://phabricator.intern.facebook.com/diffusion/FBS/browse/master/fbcode/glean/schema"},"fbcode/glean/schema"),", and those files are automatically sync'd to\nwww too.")),(0,a.mdx)("p",null,"The relationship between schema types and Thrift/JSON is given by the following table:"),(0,a.mdx)("table",null,(0,a.mdx)("thead",{parentName:"table"},(0,a.mdx)("tr",{parentName:"thead"},(0,a.mdx)("th",{parentName:"tr",align:null},"Schema type"),(0,a.mdx)("th",{parentName:"tr",align:null},"Thrift type"),(0,a.mdx)("th",{parentName:"tr",align:null},"JSON"))),(0,a.mdx)("tbody",{parentName:"table"},(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"nat")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"Nat")," (",(0,a.mdx)("inlineCode",{parentName:"td"},"i64"),")"),(0,a.mdx)("td",{parentName:"tr",align:null},"123")),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"byte")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"Byte")," (",(0,a.mdx)("inlineCode",{parentName:"td"},"i8"),")"),(0,a.mdx)("td",{parentName:"tr",align:null},"123")),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"string")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"string")),(0,a.mdx)("td",{parentName:"tr",align:null},'"abc"')),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"bool")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"bool")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"true")," or ",(0,a.mdx)("inlineCode",{parentName:"td"},"false"))),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"[byte]")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"binary")),(0,a.mdx)("td",{parentName:"tr",align:null},"base-64 encoded string ",(0,a.mdx)("sup",null,"*1"))),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"[T]")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"list<T>")),(0,a.mdx)("td",{parentName:"tr",align:null},"[...]")),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"{"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"f\u2081 : T\u2081,"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"...,"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"f\u2099 : T\u2099"),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"}")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"struct Foo {"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"1: T\u2081 f\u2081;"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"..."),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"n: T\u2099 f\u2099;"),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"}")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"{"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},'"f\u2081" : q\u2081,'),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"..."),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},'"f\u2099" : q\u2099'),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"}"))),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"{"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"f\u2081 : T\u2081 "),(0,a.mdx)("code",null,"|"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"... "),(0,a.mdx)("code",null,"|"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"f\u2099 : T\u2099"),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"}")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"union Foo {"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"1: T\u2081 f\u2081;"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"..."),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"n: T\u2099 f\u2099;"),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"}")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},'{ "f" : t }'),(0,a.mdx)("br",null),"for one of the fields ",(0,a.mdx)("inlineCode",{parentName:"td"},"f\u2081"),"..",(0,a.mdx)("inlineCode",{parentName:"td"},"f\u2099"))),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"maybe T")),(0,a.mdx)("td",{parentName:"tr",align:null},"In a record field:",(0,a.mdx)("br",null)," ",(0,a.mdx)("inlineCode",{parentName:"td"},"optional T f")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"f : t"),(0,a.mdx)("br",null)," if the value is present")),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"enum {"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"L\u2081"),(0,a.mdx)("code",null,"|"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"..."),(0,a.mdx)("code",null,"|"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"L\u2099"),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"}")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"enum Foo { "),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"L\u2081 = 1,"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"..."),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"L\u2099 = n"),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"}")),(0,a.mdx)("td",{parentName:"tr",align:null},"the index of the value,",(0,a.mdx)("br",null)," e.g. 12")),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"predicate P : K -> V")),(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"struct P {"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"1: Id id"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"2: optional K key"),(0,a.mdx)("br",null),"\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},"3: optional V value"),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"}"),(0,a.mdx)("br",null),"note",(0,a.mdx)("sup",null,"*2")),(0,a.mdx)("td",{parentName:"tr",align:null},"refer to fact N:",(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"N")," or ",(0,a.mdx)("inlineCode",{parentName:"td"},'{ "id": N }'),(0,a.mdx)("br",null),"define a fact:",(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},'{ "id" : N,'),(0,a.mdx)("br",null),"\xa0","\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},'"key" : t }')," or",(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},'{ "key": t }')," or",(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},'{ "key": t,'),(0,a.mdx)("br",null),"\xa0","\xa0","\xa0","\xa0",(0,a.mdx)("inlineCode",{parentName:"td"},'"value" : v }'))),(0,a.mdx)("tr",{parentName:"tbody"},(0,a.mdx)("td",{parentName:"tr",align:null},(0,a.mdx)("inlineCode",{parentName:"td"},"type N = T")),(0,a.mdx)("td",{parentName:"tr",align:null},"depending on T: ",(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"struct N { .. }"),(0,a.mdx)("br",null)," ",(0,a.mdx)("inlineCode",{parentName:"td"},"union N {...}"),(0,a.mdx)("br",null)," ",(0,a.mdx)("inlineCode",{parentName:"td"},"enum N {...}"),(0,a.mdx)("br",null),(0,a.mdx)("inlineCode",{parentName:"td"},"typedef T N;")),(0,a.mdx)("td",{parentName:"tr",align:null},"same as type T")))),(0,a.mdx)("ol",null,(0,a.mdx)("li",{parentName:"ol"},(0,a.mdx)("p",{parentName:"li"},"The Thrift encoding of a binary field in JSON is a base-64-encoded string. However, not all Thrift implementations respect this. At the time of writing, the Python Thrift implementation doesn't base-64-encode binary values. For this reason we provide an option in the Glean Thrift API to disable base-64 encoding for binary if your client doesn't support it. The Glean Shell also uses this option to make it easier to work with binary.")),(0,a.mdx)("li",{parentName:"ol"},(0,a.mdx)("p",{parentName:"li"},"the ",(0,a.mdx)("inlineCode",{parentName:"p"},"key")," is optional - a nested fact may be expanded in place or represented by a reference to the fact ID only. When querying Glean data the query specifies which nested facts should be expanded in the result, and when writing data to Glean using Thrift or JSON, we can optionally specify the value of nested facts inline."))))}p.isMDXComponent=!0},47596:function(e,n,t){var r=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(a,i){function d(e){try{l(r.next(e))}catch(n){i(n)}}function o(e){try{l(r.throw(e))}catch(n){i(n)}}function l(e){var n;e.done?a(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(d,o)}l((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.getSpecInfo=void 0;const a=t(11737);n.getSpecInfo=function(e){return r(this,void 0,void 0,(function*(){return yield a.call({module:"bloks",api:"getSpecInfo",args:{styleId:e}})}))}},11737:function(e,n){var t=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(a,i){function d(e){try{l(r.next(e))}catch(n){i(n)}}function o(e){try{l(r.throw(e))}catch(n){i(n)}}function l(e){var n;e.done?a(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(d,o)}l((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.call=void 0;let r=!1,a=0;const i={};n.call=function(e){return t(this,void 0,void 0,(function*(){if("staticdocs.thefacebook.com"!==window.location.hostname&&"localhost"!==window.location.hostname)return Promise.reject(new Error("Not running on static docs"));r||(r=!0,window.addEventListener("message",(e=>{if("static-docs-bridge-response"!==e.data.event)return;const n=e.data.id;n in i||console.error(`Recieved response for id: ${n} with no matching receiver`),"response"in e.data?i[n].resolve(e.data.response):i[n].reject(new Error(e.data.error)),delete i[n]})));const n=a++,t=new Promise(((e,t)=>{i[n]={resolve:e,reject:t}})),d={event:"static-docs-bridge-call",id:n,module:e.module,api:e.api,args:e.args},o="localhost"===window.location.hostname?"*":"https://www.internalfb.com";return window.parent.postMessage(d,o),t}))}},24855:function(e,n,t){var r=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(a,i){function d(e){try{l(r.next(e))}catch(n){i(n)}}function o(e){try{l(r.throw(e))}catch(n){i(n)}}function l(e){var n;e.done?a(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(d,o)}l((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.reportFeatureUsage=n.reportContentCopied=void 0;const a=t(11737);n.reportContentCopied=function(e){return r(this,void 0,void 0,(function*(){const{textContent:n}=e;try{yield a.call({module:"feedback",api:"reportContentCopied",args:{textContent:n}})}catch(t){}}))},n.reportFeatureUsage=function(e){return r(this,void 0,void 0,(function*(){const{featureName:n,id:t}=e;console.log("used feature");try{yield a.call({module:"feedback",api:"reportFeatureUsage",args:{featureName:n,id:t}})}catch(r){}}))}},44256:function(e,n,t){var r=this&&this.__createBinding||(Object.create?function(e,n,t,r){void 0===r&&(r=t),Object.defineProperty(e,r,{enumerable:!0,get:function(){return n[t]}})}:function(e,n,t,r){void 0===r&&(r=t),e[r]=n[t]}),a=this&&this.__setModuleDefault||(Object.create?function(e,n){Object.defineProperty(e,"default",{enumerable:!0,value:n})}:function(e,n){e.default=n}),i=this&&this.__importStar||function(e){if(e&&e.__esModule)return e;var n={};if(null!=e)for(var t in e)"default"!==t&&Object.prototype.hasOwnProperty.call(e,t)&&r(n,e,t);return a(n,e),n};Object.defineProperty(n,"__esModule",{value:!0}),n.OssOnly=n.FbInternalOnly=n.getEphemeralDiffNumber=n.hasEphemeralDiffNumber=n.isInternal=n.validateFbContentArgs=n.fbInternalOnly=n.fbContent=n.inpageeditor=n.feedback=n.uidocs=n.bloks=void 0,n.bloks=i(t(47596)),n.uidocs=i(t(17483)),n.feedback=i(t(24855)),n.inpageeditor=i(t(27312));const d=["internal","external"];function o(e){return m(e),u()?"internal"in e?l(e.internal):[]:"external"in e?l(e.external):[]}function l(e){return"function"==typeof e?e():e}function m(e){if("object"!=typeof e)throw new Error(`fbContent() args must be an object containing keys: ${d}. Instead got ${e}`);if(!Object.keys(e).find((e=>d.find((n=>n===e)))))throw new Error(`No valid args found in ${JSON.stringify(e)}. Accepted keys: ${d}`);const n=Object.keys(e).filter((e=>!d.find((n=>n===e))));if(n.length>0)throw new Error(`Unexpected keys ${n} found in fbContent() args. Accepted keys: ${d}`)}function u(){try{return Boolean(!1)}catch(e){return console.log("process.env.FB_INTERNAL couldn't be read, maybe you forgot to add the required webpack EnvironmentPlugin config?",e),!1}}function c(){try{return null}catch(e){return console.log("process.env.PHABRICATOR_DIFF_NUMBER couldn't be read, maybe you forgot to add the required webpack EnvironmentPlugin config?",e),null}}n.fbContent=o,n.fbInternalOnly=function(e){return o({internal:e})},n.validateFbContentArgs=m,n.isInternal=u,n.hasEphemeralDiffNumber=function(){return Boolean(c())},n.getEphemeralDiffNumber=c,n.FbInternalOnly=function(e){return u()?e.children:null},n.OssOnly=function(e){return u()?null:e.children}},27312:function(e,n,t){var r=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(a,i){function d(e){try{l(r.next(e))}catch(n){i(n)}}function o(e){try{l(r.throw(e))}catch(n){i(n)}}function l(e){var n;e.done?a(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(d,o)}l((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.submitDiff=void 0;const a=t(11737);n.submitDiff=function(e){return r(this,void 0,void 0,(function*(){const{file_path:n,new_content:t,project_name:r,diff_number:i}=e;try{return yield a.call({module:"inpageeditor",api:"createPhabricatorDiffApi",args:{file_path:n,new_content:t,project_name:r,diff_number:i}})}catch(d){throw new Error(`Error occurred while trying to submit diff. Stack trace: ${d}`)}}))}},17483:function(e,n,t){var r=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(a,i){function d(e){try{l(r.next(e))}catch(n){i(n)}}function o(e){try{l(r.throw(e))}catch(n){i(n)}}function l(e){var n;e.done?a(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(d,o)}l((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.getApi=n.docsets=void 0;const a=t(11737);n.docsets={BLOKS_CORE:"887372105406659"},n.getApi=function(e){return r(this,void 0,void 0,(function*(){const{name:n,framework:t,docset:r}=e;return yield a.call({module:"uidocs",api:"getApi",args:{name:n,framework:t,docset:r}})}))}}}]);