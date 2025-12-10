# Generic Glean LSP Server

`glean-lsp` is a generic LSP server that provides a minimal set of LSP
features using Glean data. A Glean-based LSP is particularly suited
for navigating very large codebases, where it wouldn't be practical to
use a traditional language service that parses all the source files on
the fly.

The Glean LSP server works for any language supported by Glean and
Glass, and currently provides the following LSP features:

* Go to Definition
* Go to References
* Type on Hover
* Outline
* Symbol Search

More features may be added in the future.

The LSP server is purely static, in that it will not re-index the
source files if they are edited. To update the Glean data, currently
you have to manually re-run the indexer yourself. This can be done
without restarting VS Code or the LSP server, though.

## Building & installing

`cabal install glean-lsp`

## Using it with VS Code

First index your source code into a Glean DB. This will be something like

```
cd SRC
glean index LANGUAGE --db-root GLEANDB --db NAME/HASH .
```

where

* `SRC` is the directory containing your source code
* `LANGUAGE` is the language you want to index, see the [Glean manual](https://glean.software/docs/indexer/intro/) for available languages
* `GLEANDB` is where you want to store your DBs
* `NAME' is the name for the DB, and `HASH` can just be 0 or you could
  use the git hash of the repository if you want to index multiple
  versions of the code.

See the [Glean manual](https://glean.software/docs/indexer/intro/) for any language-specific options that you might need.

To use this LSP server with VS Code you need a generic LSP client such
as [Generic LSP Client (v2)](https://github.com/zsol/vscode-glspc). Using that client,
add the following to `.vscode/settings.json`


```
[
    "glean-lsp": {
        "repo": "NAME"
    },
    "glspc.server.command": "GLEAN-LSP"
    "glspc.server.commandArguments": ["--db-root", "GLEANDB"],
    "glspc.server.languageId": [
      "LANGUAGE-ID"
    ]
}
```

where

* `GLEAN-LSP` is the path to the `glean-lsp` binary, which might be somethihng like `$HOME/.cabal/bin/glean-lsp` if you installed it with `cabal install glean-lsp`.
* `GLEANDB` is the path to your Glean DBs (same as when running the indexer above).
* `NAME` is the name of the Glean DB you want to use. Don't add
  `HASH` here, `glean-lsp` will always use the most recent version of
  the DB.
* `LANGUAGE-ID` is the language you want to enable this extension
  for. Make sure you also disable any other extensions you have for
  the same language, you can do that locally for this workspace in VS Code.

Now if you open this workspace in VS Code and open one of the source
files that was indexed, you should have go-to-definition and the other
features available.

## Example: browse the React codebase with Glean

Clone a copy of React:

```
$ git clone --depth 1 https://github.com/facebook/react.git
$ cd react
```

Start a Glean server:

```
$ glean-server --db-root .glean --port 1234 &
```

If you don't have `Flow` installed:

```
$ sudo npm install -g flow-bin
```

Index the code into a DB called `react/0`:

```
$ grep -v '^%' scripts/flow/config/flowconfig >.flowconfig
$ glean index flow . --write-root "" --service localhost:1234 --db react/0
```

Put the following into `/tmp/react/.vscode/settings`:

```
{
    "glean-lsp": {
        "repo": "react"
    },
    "glspc.server.command": "glean-lsp",
    "glspc.server.commandArguments": ["--service", "localhost:1234"],
    "glspc.server.languageId": [
      "typescript", "javascript"
    ]
}
```

Start VS Code, and open the folder `react` that you cloned. You
probably want to disable the built-in TypeScript extensions to avoid
conflicts: `Ctrl-Shift-X` and filter on built-in extensions (Show
Built-in Extensions in the `...` dropdown), then disable the
extensions for TypeScript for this workspace only.

Now open a `.js` file, and you should have navigation features
provided by `glean-lsp`: try right-click and "Go to Definition" or "Go
to References". Check that the Outline view is populated. Try symbol
search with `Ctrl-T`.

## Troubleshooting

In the "Output" pane, select the output for the LSP client in the
dropdown on the right to see the debugging output.

A common problem is the file paths not matching up, which will result
in the LSP server starting up and appearing to be working, but
go-to-definition just doesn't work. Make sure that in the Glean DB the
file paths are all relative to the root of the workspace. To check
this you can open the DB in the shell and query for `src.File _`. For
example when I look at my DB `stackage/1` which contains all the
Haskell packages in Stackage:

```
$ glean shell --db-root ~/gleandb --db stackage/1
Glean Shell, built on 2025-07-14 13:39:35.711312749 UTC, from rev <unknown>
Using local DBs from rocksdb:/home/simon/gleandb
type :help for help.
stackage> :limit 5
stackage> src.File _
{ "id": 66477, "key": "AC-Angle-1.0/Data/Angle.hs" }
{ "id": 19929124, "key": "ANum-0.2.0.2/src/Data/ANum.hs" }
{ "id": 7772678, "key": "Agda-2.6.4/src/full/Agda/Auto/Auto.hs" }
{ "id": 15530734, "key": "Agda-2.6.4/src/full/Agda/Auto/CaseSplit.hs" }
{ "id": 11442673, "key": "Agda-2.6.4/src/full/Agda/Auto/Convert.hs" }

5 results, 5 facts, 151.82ms, 208003368 bytes, 584 compiled bytes
results truncated (current limit 5, use :limit <n> to change it)
Use :more to see more results
stackage> 
```

Check that the paths look reasonable. If not, you may need to use
language-specific options to get the indexer to generate the right
paths.

If the paths look OK, things should work to the extent that support
for the language is implemented in Glean's `codemarkup` layer and the
Glass server. Some languages might be missing certain features, but
the basics should work for most languages. If you're still having
trouble try raising an issue on the [Glean issue
tracker](https://github.com/facebookincubator/Glean/issues) or asking
on Discord, details of which are in Glean's
[README](https://github.com/facebookincubator/Glean/blob/main/README.md).
