---
id: server
title: Running the Glean Server
sidebar_label: Running the Glean Server
---

import Facebook from "./fb/server.md"

Typically the Glean server is invoked as:

```
glean-server --db-root <dir> --schema <schema> --port <port>
```

Where `<dir>` is where your Glean databases are stored, `<port>` is
the port number, and `<schema>` is the directory containing the schema
sources.

The Glean server accepts all the common options described in [Common options](running.md#common-options), and additionally:

* `--port PORT`<br />
Port number to listen on.

The server watches for changes in any [configuration
files](./running.md#configuration-files) specified with `config:PATH`,
including the schema.

<Facebook />
