---
id: running
title: Running the Tools
sidebar_label: Running the Tools
---

import {fbContent, OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {Alt} from '@site/utils.js';

## Configuration files

Glean tools use some configuration files. Mostly these are optional,
if you don't provide the file then a set of defaults will be assumed.

Config files are represented as JSON-encoded Thrift values. Each
configuration file has a corresponding Thrift definition file, which
we'll link to in the docs below.

The location of a config file is specified by a command line
option, e.g. Glean clients have a `--client-config` option:

```
--client-config (file:PATH | config:PATH)
```

the alternatives are:

* Omit the flag: default values will be used.
* `--client-config file:PATH` means the config is read, once, from `PATH`.

<FbInternalOnly>

* `--client-config config:PATH` means the config is read from `PATH`
  in configerator.

</FbInternalOnly>

<OssOnly>

* `--client-config config:PATH` means the config is read from `DIR/PATH`
  where `DIR` defaults to `$HOME/.config/glean` but can be changed with the `--config-dir DIR` flag. Additionally, changes in the file are picked up without restarting the tool. (this is particularly useful for the server)

The idea is that if you're running a fleet of Glean servers, you can
have a set of configuration files that you sync to all the hosts in
your fleet using whatever mechanism you want, and set `--config-dir`
to point to the location of the files.  If this isn't sufficient, then
you can implement your own `ConfigProvider` instance to support
whatever method you need for accessing config files.

</OssOnly>


## Common options

All the Glean tools (`glean`, `glean-server`) accept
the following options.

<OssOnly>

* `--config-dir DIR`<br />
The base directory where [configuration files](#configuration-files) are read from.

</OssOnly>

### Connecting to a remote server

<FbInternalOnly>

* `--service TIER or HOST:PORT`<br />
**Default:** taken from `--client-config`<br />
Specifies the Glean server to connect to.  `TIER` is
implementation-dependent (not supported in the open-source build
currently).

</FbInternalOnly>

<OssOnly>

* `--service HOST:PORT`<br />
**Default:** taken from `--client-config`<br />
Specifies the Glean server to connect to.

</OssOnly>

* `--client-config (file:PATH | config:PATH)`<br />
**Default:**<Alt internal="config:glean/client/client" external="config:client" /> or use default values if missing<br />
**Thrift file:** [client-config.thrift](https://github.com/facebookincubator/Glean/blob/master/glean/config/client/client_config.thrift)<br />
The location of the client configuration file, which specifies the
default Glean server to connect to, amongst other things.

<FbInternalOnly>

* `--use-shards yes|no|fallback`<br />
FB-only: whether to use shards when connecting to a host in the tier.

</FbInternalOnly>

### Using local databases

* `--db-root DIR`<br />
The path where Glean databases are stored.

* `--schema (file:FILE | dir:DIR | config:PATH | DIR)`<br />
**Default**: <Alt internal="config:glean/schema/all" external="config:schema" /><br />
The location of the schema definition. This can either be:
  * `dir:DIR` or just `DIR`<br />
    All the files with the extension `.angle` under `DIR` (or in
    subdirectories) will be read.
  * `file:FILE` or `config:PATH`<br/>
    A single file, which can be created from the source files in `DIR` by running `gen-schema --dir DIR --source FILE`<br />
    If you are running a fleet of Glean servers, you would normally
    sync schema changes across the fleet by putting the schema
    in `config:schema`.

* `--schema-version VERSION`<br />

<FbInternalOnly>

* `--recipe-config (file:PATH | config:PATH)`<br />
**Default:** <Alt internal="config:glean/recipes/recipes" external="config:recipes" /> or use default values if missing<br />
**Thrift file:** [recipe-config.thrift](https://github.com/facebookincubator/Glean/blob/master/glean/config/recipes/recipes.thrift)<br />
The location of the recipes config file. This is used by certain
indexers to specify the indexing steps and dependencies between them.

</FbInternalOnly>

* `--server-config (file:PATH | config:PATH)`<br />
**Default**: <Alt internal="config:glean/server" external="config:server" /><br />
**Thrift file:** [server-config.thrift](https://github.com/facebookincubator/Glean/blob/master/glean/config/server/server_config.thrift)

* `--tier TIER`<br />
Equivalent to <Alt internal="--server-config config:glean/server/TIER" external="--server-config config:server/TIER" /><br />
Can be handy when you have fleet-wide configuration files but you
want certain servers to have different configs.

* `--db-read-only`<br />
Disable writing to databases. This is useful if you have servers that only
serve queries, and should not be creating or writing databases.

<FbInternalOnly>

* `--enable-logging`<br />
Enable logging of all operations to Scuba.

</FbInternalOnly>

<OssOnly>

* `--enable-logging`<br />
Enable logging of all operations. (Note that there is currently no
logging backend implemented for the open-source build of Glean, so
this doesn't do anything yet).

</OssOnly>

### Testing options

* `--db-schema-override`<br />
The current schema will override the schema in the
database. Normally you don't want this, because the schema stored in
the database is the one that was active at the time when the DB was
created, so it is likely to be a correct description of the data in
the database.

* `--db-mock-writes`<br />
Allow write operations, but discard the data and don't write it to the DB.
