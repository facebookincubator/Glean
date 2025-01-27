---
id: walkthrough
title: Walkthrough
sidebar_label: Walkthrough
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';

<OssOnly>

We can play with Glean using the [shell](shell.md). You can do this
directly from the [Docker image](trying.md) if you want, or [Build Glean
from source](./building.md) first.

</OssOnly>

<FbInternalOnly>

We can play with Glean using the [shell](shell.md).

</FbInternalOnly>

To try experiments we can work with a local schema definition and
local database (as opposed to connecting to a Glean server).  If you
want to play along with the examples, you can do so as follows:

<OssOnly>

```lang=sh
mkdir /tmp/glean
mkdir /tmp/glean/db
mkdir /tmp/glean/schema
glean shell --db-root /tmp/glean/db --schema /tmp/glean/schema
```

Put the [example
schema](https://github.com/facebookincubator/Glean/blob/master/glean/example/schema/example.angle)
in `/tmp/glean/schema/example.angle` and the [example
data](https://github.com/facebookincubator/Glean/blob/master/glean/example/facts.glean)
in `/tmp/glean/facts.glean`. Then reload schema and create a database from the example
data using `:reload` and `:load <file>` in the shell:

</OssOnly>

<FbInternalOnly>

```lang=sh
cd fbcode
glean shell --db-root /tmp/glean --schema glean/example/schema
```

Then create a database from the example data with `:load <file>` in the shell:

</FbInternalOnly>

```lang=sh
> :load glean/example/facts.glean
facts>
```

Now head over to [Angle Guide](angle/guide.md) to try some example
queries and learn about how the query language works.
