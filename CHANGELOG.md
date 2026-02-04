# Revision history for glean

## 0.2.0.0

* Added `glean-lsp`, a multi-language LSP server based on Glean
* Added a new Haskell indexer which consumes `.hie` files directly and
  collects much richer data than the old indexer.
* Added a new experimental DB backend based on LMDB, which at
  least for some benchmarks performs 30-40% faster than RocksDB. Add
  the `--lmdb` flag to use it.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
