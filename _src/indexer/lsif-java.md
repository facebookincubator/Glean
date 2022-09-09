---
id: lsif-java
title: Java
sidebar_label: Java
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

To index Java we use
[lsif-java](https://sourcegraph.github.io/scip-java/) in LSIF mode from
SourceGraph. This indexer supports Java, Kotlin and Scala indexing, however we
have only tested with Java so far.  Maven and Gradle are well supported.

You can download the [latest
lsif-java](https://search.maven.org/artifact/com.sourcegraph/lsif-java_2.13)
from Maven.org central. Example installation using coursier. You will need a JDK
and Maven or Gradle installed to build, as lsif-java is a plugin to the Java
compiler.

```
curl -fLo coursier https://git.io/coursier-cli && chmod +x coursier
./coursier bootstrap --standalone -o lsif-java com.sourcegraph:lsif-java_2.13:0.8.0-RC1 --main-class com.sourcegraph.lsif_java.LsifJava
```


## Run the indexer

The indexer can be run via the main `glean` CLI tool.

```
> cabal build exe:glean
```

And index your Java repository with:
```
glean index java-lsif DIR --db NAME/INSTANCE
```

where

* `DIR` is the root directory containing the Rust project
* `name/hash` is the name of the repository to create

Provide the usual `--db-root` and `--schema` or `--service` arguments
to `glean`

## In the shell

Java source can also be indexed directly from the Glean shell:

```
:index java-lsif DIR
```

The shell will pick a DB name and hash for you based on `DIR`.
You can also run lsif-java offline, and then :load the resulting lsif file into
the shell.

## Schema

The schema is in <SrcFile file="glean/schema/source/lsif.angle" />
