import React from 'react';
import {isInternal,fbContent} from 'internaldocs-fb-helpers';

let url;
if (isInternal()) {
    url = "https://www.internalfb.com/code/fbsource/fbcode/";
} else {
    url = "https://github.com/facebookincubator/Glean/tree/master/" ;
};

export function SrcFile(props) {
  return <a href={url + props.file}>{props.file}</a>;
};

export function SrcFileLink(props) {
  return <a href={url + props.file}>{props.children}</a>;
};

export const Alt = ({children, internal, external}) => (
  fbContent({
     internal: <code>{internal}</code>,
     external: <code>{external}</code>
  })
);
