# Glass: a language agnostic symbol server using Glean

Glass is a Thrift service implementing symbol search, navigation, find-references and other code symbol operations.
Major clients are CodeHub and various VS Code extensions. Glass provides an
LSP-like API to Glean, that is also language-agnostic.

## API

Defined in [glass.thrift](https://www.internalfb.com/code/fbsource/fbcode/glean/glass/if/glass.thrift) , the main operations are:

- list document symbols and references
- find-references
- search for matching symbols
- describe all metadata about a symbol

## What languages are supported?

Most major languages at Meta are supported, including Python, C++, Hack and JavaScript.
The current repository and language mappings we support are listed in configuration table in
fbcode/glean/glass/facebook/Glean/Glass/RepoMapping.hs

## What clients are supported

Thrift, Hack bindings to Thrift, and the C++, Rust and Python bindings are most common.
The `glass` cli is also on most devservers for human use.

## More info

- Conveyor https://www.internalfb.com/svc/services/glean/glass/conveyor/glean/glass/releases
- Development hints and tips https://www.internalfb.com/intern/wiki/Glean/Glass/Developing\_Glass/
- Glass logs and data sets https://www.internalfb.com/intern/wiki/Glean/Glass/Infrastructure/

## API

{{#codehub_symbol_card fbsource/thrift/fbcode/glean/glass/if/glass.thrift/GlassService}}
