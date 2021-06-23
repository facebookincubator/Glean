---
id: server
title: Running the Glean Server
sidebar_label: Running the Glean Server
---

## Running a local Glean server

For testing derived schemas or indexers, it can be useful to run a local Glean server.
From an fbcode checkout,
```
> buck build //glean/server:server
```
Then, to run on your dev server, using the local schema definitions:

```
> buck run glean/server:server -- --db-root=$HOME/mygleandb  --db-schema-override dir:$HOME/fbsource/fbcode/glean/schema/
```

You can write to such a server from an ondemand via host:port Glean configs.
