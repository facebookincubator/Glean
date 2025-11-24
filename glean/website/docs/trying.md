---
id: trying
title: Trying Glean
sidebar_label: Trying Glean
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers'; import
{SrcFile,SrcFileLink} from '@site/utils';

:::note

Glean only runs on **x86_64 Linux** and **aarch64 Linux** at this time.

:::

We publish weekly Docker images of Glean for x86_64 and aarch64 Linux that you
can use, including a pre-built demo image with a Glean database, containing an
imported snapshot of the [React] repository. (These images are built
automatically by a [Github Action][docker-gha].) Run it immediately with:

```
docker run -it --rm \
  -p 12345:12345 -p 8888:8888 \
  --name glean-server \
  ghcr.io/facebookincubator/glean/demo:latest
```

Then, visit [`http://localhost:8888`](http://localhost:8888) in your browser,
and you should see a list of source files for React. Click on a file, and
navigate around the code by clicking on a symbol reference to jump to its
definition. Try something substantial like
<http://localhost:8888/packages/react-dom/src/client/ReactDOMComponent.js> and
note how Glean is accurately linking both local and imported symbols.

This demo is powered by the <SrcFileLink
file="glean/demo/Hyperlink.hs">glean-hyperlink</SrcFileLink> tool.

[React]: https://react.dev
[docker-gha]: https://github.com/facebookincubator/Glean/blob/master/.github/workflows/docker.yml

### Connecting with the shell

Open a new terminal, and open the Glean [shell](shell.md) by pointing it at the
server you started:

```
docker run -it --rm \
  --link glean-server ghcr.io/facebookincubator/glean/client:latest \
  glean shell --service glean-server:12345
```

You should see:

```
Glean Shell, built on <time>, from rev <unknown>
Using service at glean-server:12345
type :help for help.
>
```

You can see the pre-generated database containing the results of running the
Flow indexer on React:

```
> :list
react/0 (complete)
  Created: 2025-01-23 20:03:35 UTC (1 minute ago)
  Completed: 2025-01-23 20:03:48 UTC (1 minute ago)
```

We can look at the contents:

```
react> :db react
using database react/0
react> :stat
flow.Declaration.3
  count: 34423
  size:  1150802 (1.10 MiB) 4.9262%
...

Total: 622052 facts (22.28 MiB)
```

## Connecting with the shell (locally)

The previous example connected to Glean over the network using Docker's
networking functionality. But you can also start Glean locally inside the
container.

```
docker run -it --rm ghcr.io/facebookincubator/glean/demo:latest bash
```

Then start the shell, pointing it to the proper schema and database directories:

```
glean shell --db-root /glean/db --schema /glean/schema
```

The commands work exactly the same as with remote databases, but you will need
to use `docker run -v` to mount your local database and schema directories in
this case.
