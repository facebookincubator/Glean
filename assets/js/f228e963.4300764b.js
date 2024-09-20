"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[2982],{15680:(e,n,t)=>{t.r(n),t.d(n,{MDXContext:()=>d,MDXProvider:()=>f,mdx:()=>v,useMDXComponents:()=>s,withMDXComponents:()=>u});var r=t(96540);function o(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(){return i=Object.assign||function(e){for(var n=1;n<arguments.length;n++){var t=arguments[n];for(var r in t)Object.prototype.hasOwnProperty.call(t,r)&&(e[r]=t[r])}return e},i.apply(this,arguments)}function a(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);n&&(r=r.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,r)}return t}function l(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?a(Object(t),!0).forEach((function(n){o(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):a(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function c(e,n){if(null==e)return{};var t,r,o=function(e,n){if(null==e)return{};var t,r,o={},i=Object.keys(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||(o[t]=e[t]);return o}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(o[t]=e[t])}return o}var d=r.createContext({}),u=function(e){return function(n){var t=s(n.components);return r.createElement(e,i({},n,{components:t}))}},s=function(e){var n=r.useContext(d),t=n;return e&&(t="function"==typeof e?e(n):l(l({},n),e)),t},f=function(e){var n=s(e.components);return r.createElement(d.Provider,{value:n},e.children)},p="mdxType",m={inlineCode:"code",wrapper:function(e){var n=e.children;return r.createElement(r.Fragment,{},n)}},h=r.forwardRef((function(e,n){var t=e.components,o=e.mdxType,i=e.originalType,a=e.parentName,d=c(e,["components","mdxType","originalType","parentName"]),u=s(t),f=o,p=u["".concat(a,".").concat(f)]||u[f]||m[f]||i;return t?r.createElement(p,l(l({ref:n},d),{},{components:t})):r.createElement(p,l({ref:n},d))}));function v(e,n){var t=arguments,o=n&&n.mdxType;if("string"==typeof e||o){var i=t.length,a=new Array(i);a[0]=h;var l={};for(var c in n)hasOwnProperty.call(n,c)&&(l[c]=n[c]);l.originalType=e,l[p]="string"==typeof e?e:o,a[1]=l;for(var d=2;d<i;d++)a[d]=t[d];return r.createElement.apply(null,a)}return r.createElement.apply(null,t)}h.displayName="MDXCreateElement"},67555:(e,n,t)=>{t.d(n,{D4:()=>a,Lw:()=>l,gD:()=>c});var r=t(96540),o=t(14423);let i;function a(e){return r.createElement("a",{href:i+e.file},e.file)}function l(e){return r.createElement("a",{href:i+e.file},e.children)}i=(0,o.isInternal)()?"https://www.internalfb.com/code/fbsource/fbcode/":"https://github.com/facebookincubator/Glean/tree/master/";const c=e=>{let{children:n,internal:t,external:i}=e;return(0,o.fbContent)({internal:r.createElement("code",null,t),external:r.createElement("code",null,i)})}},97352:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>d,contentTitle:()=>l,default:()=>p,frontMatter:()=>a,metadata:()=>c,toc:()=>u});var r=t(58168),o=(t(96540),t(15680)),i=(t(14423),t(67555));const a={id:"flow",title:"JavaScript (Flow)",sidebar_label:"JavaScript (Flow)"},l=void 0,c={unversionedId:"indexer/flow",id:"indexer/flow",title:"JavaScript (Flow)",description:"The JavaScript/Flow indexer is built into the",source:"@site/docs/indexer/flow.md",sourceDirName:"indexer",slug:"/indexer/flow",permalink:"/docs/indexer/flow",draft:!1,editUrl:"https://github.com/facebookincubator/Glean/tree/main/glean/website/docs/indexer/flow.md",tags:[],version:"current",frontMatter:{id:"flow",title:"JavaScript (Flow)",sidebar_label:"JavaScript (Flow)"},sidebar:"someSidebar",previous:{title:"C++ and C",permalink:"/docs/indexer/cxx"},next:{title:"Hack",permalink:"/docs/indexer/hack"}},d={},u=[{value:"Run the indexer",id:"run-the-indexer",level:2},{value:"Run the indexer (manually)",id:"run-the-indexer-manually",level:2},{value:"Derived predicates",id:"derived-predicates",level:2},{value:"In the shell",id:"in-the-shell",level:2},{value:"Schema",id:"schema",level:2}],s={toc:u},f="wrapper";function p(e){let{components:n,...t}=e;return(0,o.mdx)(f,(0,r.A)({},s,t,{components:n,mdxType:"MDXLayout"}),(0,o.mdx)("p",null,"The JavaScript/Flow indexer is built into the\n",(0,o.mdx)("a",{parentName:"p",href:"https://flow.org/"},"Flow")," system.  It's also included\nin the ",(0,o.mdx)("a",{parentName:"p",href:"/docs/trying"},"Glean demo Docker image")," to try out."),(0,o.mdx)("h2",{id:"run-the-indexer"},"Run the indexer"),(0,o.mdx)("p",null,"The indexer is run via the main ",(0,o.mdx)("inlineCode",{parentName:"p"},"glean")," CLI tool."),(0,o.mdx)("pre",null,(0,o.mdx)("code",{parentName:"pre"},"> cabal build exe:glean\n")),(0,o.mdx)("p",null,"And index your Flow repository with:"),(0,o.mdx)("pre",null,(0,o.mdx)("code",{parentName:"pre"},"glean index flow DIR --db NAME/INSTANCE\n")),(0,o.mdx)("p",null,"where"),(0,o.mdx)("ul",null,(0,o.mdx)("li",{parentName:"ul"},(0,o.mdx)("inlineCode",{parentName:"li"},"DIR")," is the root directory containing the Flow project (with ",(0,o.mdx)("inlineCode",{parentName:"li"},".flowconfig"),")"),(0,o.mdx)("li",{parentName:"ul"},(0,o.mdx)("inlineCode",{parentName:"li"},"name/hash")," is the name of the repository to create")),(0,o.mdx)("p",null,"Provide the usual ",(0,o.mdx)("inlineCode",{parentName:"p"},"--db-root")," and ",(0,o.mdx)("inlineCode",{parentName:"p"},"--schema")," or ",(0,o.mdx)("inlineCode",{parentName:"p"},"--service")," arguments\nto ",(0,o.mdx)("inlineCode",{parentName:"p"},"glean")),(0,o.mdx)("h2",{id:"run-the-indexer-manually"},"Run the indexer (manually)"),(0,o.mdx)("pre",null,(0,o.mdx)("code",{parentName:"pre"},"flow glean DIR --output-dir JSON --write-root PREFIX\n")),(0,o.mdx)("p",null,"where"),(0,o.mdx)("ul",null,(0,o.mdx)("li",{parentName:"ul"},(0,o.mdx)("inlineCode",{parentName:"li"},"DIR")," is the root directory containing the JavaScript/Flow files"),(0,o.mdx)("li",{parentName:"ul"},(0,o.mdx)("inlineCode",{parentName:"li"},"JSON")," is the directory in which to write the output ",(0,o.mdx)("inlineCode",{parentName:"li"},".json")," files"),(0,o.mdx)("li",{parentName:"ul"},(0,o.mdx)("inlineCode",{parentName:"li"},"PREFIX")," is a prefix to add to the files in the Glean index (this\ncan be empty if you don't need a prefix)")),(0,o.mdx)("p",null,"The generated files can be ingested into a Glean database using ",(0,o.mdx)("a",{parentName:"p",href:"/docs/cli#glean-create"},(0,o.mdx)("inlineCode",{parentName:"a"},"glean create")),"."),(0,o.mdx)("h2",{id:"derived-predicates"},"Derived predicates"),(0,o.mdx)("p",null,"Several predicates should be derived after indexing. For each ",(0,o.mdx)("inlineCode",{parentName:"p"},"stored")," predicate in the ",(0,o.mdx)("a",{parentName:"p",href:"#schema"},"schema")," you should ",(0,o.mdx)("a",{parentName:"p",href:"/docs/cli#glean-derive"},(0,o.mdx)("inlineCode",{parentName:"a"},"glean derive"))," the predicate."),(0,o.mdx)("h2",{id:"in-the-shell"},"In the shell"),(0,o.mdx)("p",null,"Flow source can also be indexed directly from the Glean shell:"),(0,o.mdx)("pre",null,(0,o.mdx)("code",{parentName:"pre"},":index flow DIR\n")),(0,o.mdx)("h2",{id:"schema"},"Schema"),(0,o.mdx)("p",null,"The schema is in ",(0,o.mdx)(i.D4,{file:"glean/schema/source/flow.angle",mdxType:"SrcFile"})))}p.isMDXComponent=!0},80510:function(e,n,t){var r=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(o,i){function a(e){try{c(r.next(e))}catch(n){i(n)}}function l(e){try{c(r.throw(e))}catch(n){i(n)}}function c(e){var n;e.done?o(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(a,l)}c((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.getSpecInfo=void 0;const o=t(88266);n.getSpecInfo=function(e){return r(this,void 0,void 0,(function*(){return yield o.call({module:"bloks",api:"getSpecInfo",args:{styleId:e}})}))}},88266:function(e,n){var t=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(o,i){function a(e){try{c(r.next(e))}catch(n){i(n)}}function l(e){try{c(r.throw(e))}catch(n){i(n)}}function c(e){var n;e.done?o(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(a,l)}c((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.call=void 0;let r=!1,o=0;const i={};n.call=function(e){return t(this,void 0,void 0,(function*(){if("staticdocs.thefacebook.com"!==window.location.hostname&&"localhost"!==window.location.hostname)return Promise.reject(new Error("Not running on static docs"));r||(r=!0,window.addEventListener("message",(e=>{if("static-docs-bridge-response"!==e.data.event)return;const n=e.data.id;n in i||console.error(`Recieved response for id: ${n} with no matching receiver`),"response"in e.data?i[n].resolve(e.data.response):i[n].reject(new Error(e.data.error)),delete i[n]})));const n=o++,t=new Promise(((e,t)=>{i[n]={resolve:e,reject:t}})),a={event:"static-docs-bridge-call",id:n,module:e.module,api:e.api,args:e.args},l="localhost"===window.location.hostname?"*":"https://www.internalfb.com";return window.parent.postMessage(a,l),t}))}},70680:function(e,n,t){var r=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(o,i){function a(e){try{c(r.next(e))}catch(n){i(n)}}function l(e){try{c(r.throw(e))}catch(n){i(n)}}function c(e){var n;e.done?o(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(a,l)}c((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.reportFeatureUsage=n.reportContentCopied=void 0;const o=t(88266);n.reportContentCopied=function(e){return r(this,void 0,void 0,(function*(){const{textContent:n}=e;try{yield o.call({module:"feedback",api:"reportContentCopied",args:{textContent:n}})}catch(t){}}))},n.reportFeatureUsage=function(e){return r(this,void 0,void 0,(function*(){const{featureName:n,id:t}=e;console.log("used feature");try{yield o.call({module:"feedback",api:"reportFeatureUsage",args:{featureName:n,id:t}})}catch(r){}}))}},14423:function(e,n,t){var r=this&&this.__createBinding||(Object.create?function(e,n,t,r){void 0===r&&(r=t),Object.defineProperty(e,r,{enumerable:!0,get:function(){return n[t]}})}:function(e,n,t,r){void 0===r&&(r=t),e[r]=n[t]}),o=this&&this.__setModuleDefault||(Object.create?function(e,n){Object.defineProperty(e,"default",{enumerable:!0,value:n})}:function(e,n){e.default=n}),i=this&&this.__importStar||function(e){if(e&&e.__esModule)return e;var n={};if(null!=e)for(var t in e)"default"!==t&&Object.prototype.hasOwnProperty.call(e,t)&&r(n,e,t);return o(n,e),n};Object.defineProperty(n,"__esModule",{value:!0}),n.OssOnly=n.FbInternalOnly=n.getEphemeralDiffNumber=n.hasEphemeralDiffNumber=n.isInternal=n.validateFbContentArgs=n.fbInternalOnly=n.fbContent=n.inpageeditor=n.feedback=n.uidocs=n.bloks=void 0,n.bloks=i(t(80510)),n.uidocs=i(t(3730)),n.feedback=i(t(70680)),n.inpageeditor=i(t(45458));const a=["internal","external"];function l(e){return d(e),u()?"internal"in e?c(e.internal):[]:"external"in e?c(e.external):[]}function c(e){return"function"==typeof e?e():e}function d(e){if("object"!=typeof e)throw new Error(`fbContent() args must be an object containing keys: ${a}. Instead got ${e}`);if(!Object.keys(e).find((e=>a.find((n=>n===e)))))throw new Error(`No valid args found in ${JSON.stringify(e)}. Accepted keys: ${a}`);const n=Object.keys(e).filter((e=>!a.find((n=>n===e))));if(n.length>0)throw new Error(`Unexpected keys ${n} found in fbContent() args. Accepted keys: ${a}`)}function u(){try{return Boolean(!1)}catch(e){return console.log("process.env.FB_INTERNAL couldn't be read, maybe you forgot to add the required webpack EnvironmentPlugin config?",e),!1}}function s(){try{return null}catch(e){return console.log("process.env.PHABRICATOR_DIFF_NUMBER couldn't be read, maybe you forgot to add the required webpack EnvironmentPlugin config?",e),null}}n.fbContent=l,n.fbInternalOnly=function(e){return l({internal:e})},n.validateFbContentArgs=d,n.isInternal=u,n.hasEphemeralDiffNumber=function(){return Boolean(s())},n.getEphemeralDiffNumber=s,n.FbInternalOnly=function(e){return u()?e.children:null},n.OssOnly=function(e){return u()?null:e.children}},45458:function(e,n,t){var r=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(o,i){function a(e){try{c(r.next(e))}catch(n){i(n)}}function l(e){try{c(r.throw(e))}catch(n){i(n)}}function c(e){var n;e.done?o(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(a,l)}c((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.submitDiff=void 0;const o=t(88266);n.submitDiff=function(e){return r(this,void 0,void 0,(function*(){const{file_path:n,new_content:t,project_name:r,diff_number:i}=e;try{return yield o.call({module:"inpageeditor",api:"createPhabricatorDiffApi",args:{file_path:n,new_content:t,project_name:r,diff_number:i}})}catch(a){throw new Error(`Error occurred while trying to submit diff. Stack trace: ${a}`)}}))}},3730:function(e,n,t){var r=this&&this.__awaiter||function(e,n,t,r){return new(t||(t=Promise))((function(o,i){function a(e){try{c(r.next(e))}catch(n){i(n)}}function l(e){try{c(r.throw(e))}catch(n){i(n)}}function c(e){var n;e.done?o(e.value):(n=e.value,n instanceof t?n:new t((function(e){e(n)}))).then(a,l)}c((r=r.apply(e,n||[])).next())}))};Object.defineProperty(n,"__esModule",{value:!0}),n.getApi=n.docsets=void 0;const o=t(88266);n.docsets={BLOKS_CORE:"887372105406659"},n.getApi=function(e){return r(this,void 0,void 0,(function*(){const{name:n,framework:t,docset:r}=e;return yield o.call({module:"uidocs",api:"getApi",args:{name:n,framework:t,docset:r}})}))}}}]);