# Source tree roadmap

## Components and Dependencies

This is the dependency structure of the main components, by directory:

```
                +----------+
                |          |
                |   lib    | Schema-specific libraries
                |          |
                +-----+----+
                      |
                +-----v----+
                |          |
                |  schema  | Generated schema support code
                |          |
                +-----+----+
                      |
                      |
                      |
                      |
                +-----v-----+                   +-----------------+
                |           | remote            |                 | Access
                | client/hs | client API        | client/hs/local | local DBs
                |           |                   |                 |
  +------+      +-----+-----+                   +--------+--------+
  |      |            |                                  |
  |  if  |            |                                  |
  |      |       +----v---+                         +----v---+
  +------+<------|        |                         |        |
                 |   hs   <--------------------------   db   |
  +------+<------|        |                         |        |
  |      |       +----+---+                         +----|---+
  | util |            |                                  |
  |      |            |                                  |
  +------+            |                                  |
                      |                                  |         Haskell
            ----------+----------------------------------+-----------------
                      |                                  |           C++
                      |                                  |
                 +----v----+                        +----v----+
                 |         |                        |         |
                 |   rts   <------------------------+ rocksdb |
                 |         |                        |         |
                 +---------+                        +---------+
```

* Haskell clients should depend on client/hs

* Haskell clients can optionally depende on client/hs/local if they
  want to be able to use local databases directly rather than
  connecting to a server. This pulls in the whole of the storage and
  query backend, though.


## Directories

Roughly in dependency order, from low to high:

* **rts**
  * The Glean Runtime System (C++)
    * Fact storage
    * Fact serialization and deserialization
    * Fact ownership
    * Bytecode evaluation
    * Query evaluation
    * Write cache
 * **cpp**
   * API for using Glean RTS from C++ (used by the Clang indexer)
 * **util**
   * Haskell utilities (`Glean.Util.*`)
 * **bytecode**
   * Bytecode definitions and code generation
 * **if**
   * Thrift files
 * **hs**
   * Low-level Haskell layer
     * Angle Types and Parser (`Glean.Angle.*`)
     * Schema handling (`Glean.Schema.*`)
     * Haskell API to **rts** functionality (`Glean.RTS.*`)
     * Bytecode generation (`Glean.Bytecode.*`)
     * Serialising/deserialising Glean data to Haskell types (`Glean.Typed.*`)
     * Remote Client API (`Glean.Backend.Remote`)
     * API for writing data from clients (`Glean.Write.*`)
 * **db**
    * Database management (`Glean.Database.*`)
    * Query engine (`Glean.Query.*`)
    * Write processing (`Glean.Write.*`)
    * Generic Glean backend (`Glean.Backend`)
 * **rocksdb**
    * C++ layer providing RocksDB storage for Glean data
 * **haxl**
    * Haxl data source for Glean, used in the Haskell query API
 * **client**
    * Client APIs for various languages
 * **server**
    * The server. (Note, does not depend on the schema)
 * **index**
    * Framework for specifying indexers (`Glean.Index.*`) and the
      central list of available indexers in `index/list`. This is used
      to support the `glean index` command and `:index` in the shell.
 * **schema**
    * **source**
      * The schema source
    * **gen**
      * Schema code generation
    * **thrift**, **hs** etc.
      * Generated code
 * **lib**
   * Schema-specific support libraries. (non-schema-specific libraries go
     into util instead)
 * **lang**
   * Language-specific indexers
   * Regression tests for indexers and schema code
 * **test**
   * **lib**
     * Tests libraries
   * **tests**
     * Unit / integration tests
   * **regression**
     * Regression test framework
 * **tools**
   * The Glean CLI tool, and other misc tools
 * **example**
   * Example schema and facts (used as a walkthrough in the docs)
 * **demo**
   * Small demo examples
 * **bench**
   * Benchmarks
 * **scripts**
    * Misc ad-hoc scripts
 * **glass**
   * The Glass service, provides language-independent symbol services
     (go-to-definition, find-references, symbol search etc.)  on top
     of Glean.
 * **vscode**
   * Extension for editing `.angle` files in VS Code
 * **website**
   * Web site and docs
