---
id: trying
title: Trying Glean
sidebar_label: Trying Glean
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

We provide a Docker image containing a pre-built set of Glean binaries
that you can try out.  These images are built automatically by a
[Github Action](https://github.com/facebookincubator/Glean/blob/master/.github/workflows/glean-docker.yml).

Pull the latest demo Docker image (warning, this is around 7GB):

```
docker pull ghcr.io/facebookincubator/glean/demo:latest
```

Run it:

```
docker run -it -p 8888:8888 ghcr.io/facebookincubator/glean/demo:latest
```

:::info

What's in the image?

* A build of Glean, in `/glean-code`
* The [flow](https://github.com/facebook/flow/) binary, in `/usr/local/bin/flow`
* A checkout of [react](https://github.com/facebook/react/) in `/react-code`
* A Glean database containing the Flow index of React in `/gleandb`

:::

Start the Glean [shell](shell.md):

```
glean shell --db-root /glean-demo/db --schema /glean-demo/schema/source
```

You should see:

```
Glean Shell, built on <time>, from rev <unknown>
type :help for help.
>
```

The demo image contains a pre-generated database containing the
results of running the Flow indexer on the React repository:

```
> :list
react/0 (complete)
  Created: 2021-05-24T02:42:33Z (30 days, 9 hours ago)
```

We can look at the contents:

```
react> :db react
using database react/0
react> :stat
flow.Declaration.3
  count: 26756
  size:  888756 (867.93 kB) 4.8248%
...
Total size: 17.57 MB
```

## Running the server

Above we showed the shell reading the database from the filesystem
directly. Instead we can run a server that the clients will interact
with to make queries:

```
glean-server --db-root /glean-demo/db --schema /glean-demo/schema/source --port 12345
```

And now the shell can connect to the server:

```
glean shell --service localhost:12345
```

The commands work exactly the same as with local databases, but now it
would also work over the network.

## Hyperlink demo

We have a small demo showing how Glean can enable code navigation. The <SrcFileLink file="glean/demo/Hyperlink.hs">glean-hyperlink</SrcFileLink> tool
creates a webserver that serves hyperlinked source code using data
from a specified Glean database.

We can navigate the React source code as follows. First start the
Glean server:

```
glean-server --db-root /glean-demo/db --schema /glean-demo/schema/source --port 12345
```

Next start the Hyperlink server:

```
glean-hyperlink --service localhost:12345 --db react --root /react-code --http 8888
```

Now navigate to `http://localhost:8888` in your browser, and you
should see a list of source files. Click on a file, and navigate
around the code by clicking on a symbol reference to jump to its
definition.  Try something substantial like
`react-dom/src/client/ReactDOMComponent.js`
(http://localhost:8888/packages/react-dom/src/client/ReactDOMComponent.js) -
note how Glean is accurately linking both local and imported
symbols.
