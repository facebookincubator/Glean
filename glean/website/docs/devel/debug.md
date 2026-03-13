---
id: debugging
title: Debugging
sidebar_label: Debugging
---

Some hints on how to debug Glean.

## Logging and debug output

### Glog

Glean uses [glog](https://github.com/google/glog) for a lot of its
logging both in C++ and Haskell. The Haskell API is provided by the
`Util.Log` library in the `fb-util` package
(`hsthrift/common/util`).

Glog has various logging levels, INFO, WARN, ERROR, etc.,
corresponding to `logInfo`, `logWarn` and so on in `Util.Log`, and
also verbose logging via `vlog`.

By default, verbose logs are suppressed but all other logs are shown,
except in `glean shell` where we suppress `logInfo` to avoid logs
interfering with the interactive shell. To see more logs there are
various ways. The most foolproof is with an environment variable. To
see verbose logs up to level 3 for example:

```
GLOG_v=3 glean-server ...
```

The shell is a bit different, because it adds an implicit
`--minloglevel=2` argument which overrides the environment
variable. So for the shell we must use the command-line `-v`:

```
glean -v 3 shell ...
```

Glog is sometimes a bit spammy because you'll see logs from irrelevant
library dependencies. For C++ you can limit the logs you see to just
certain files. For example, to turn on verbose logging at level 3 for
just `query.cpp`:

```
GLOG_vmodule=query=3 glean-server ...
```

This useful functionality unfortunately doesn't work for logs in
Haskell code.

### Glean debug logging

Glean also has its own debug logs which are enabled via environment
variables or command-line options. There are currently two of these:

* `GLEAN_DEBUG=query` or `--debug-query`: enable logging from the query compiler
* `GLEAN_DEBUG=tc` or `--debug-tc`: enable logging from the Angle type checker

## Debugging the C++ code

We recommend [Address
Sanitizer](https://github.com/google/sanitizers/wiki/addresssanitizer)
as a first port of call for debugging anything in C++. Address
Sanitizer will detect most kinds of memory errors and space leaks.

To make it easy to enable this we've provided a `cabal` flag:

```
cabal build glean-server -fasan
```

(Note you might need to `cabal clean` first, or selectively remove
some build files, because Cabal isn't very good at rebuilding C++ code
when it needs to. You'll need to clean again if you turn off `asan`.)

## Profiling C++

`perf` is a pretty good way to identify hotspots quickly.

```
perf record -g glean-server ...
perf report
```
