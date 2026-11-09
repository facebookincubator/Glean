# Glean

Glean is a system for collecting, storing, and querying facts about code
(code indexing and code search). This folder holds the Glean implementation:
fact schemas (`schema/`), per-language indexers (`lang/`), databases and
storage backends (`db/`, `rts/`, `rocksdb/`, `lmdb/`), client libraries
(`client/`), the Angle query language (`angle/`), and tools built on Glean
such as Glass (`glass/`).
