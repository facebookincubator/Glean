  $ source "$TESTDIR/setup.sh"
  Creating DB using handle fbcode:glean/tools/gleancli:glean@.* (re)
  $ "$SHELL" --service "::1:$PORT" --db "$DB" "example.Class _"
  [>] example.Class _ (re)
  { "id": [0-9]+, "key": { "name": "Fish", "line": 30 } } (re)
  { "id": [0-9]+, "key": { "name": "Goldfish", "line": 40 } } (re)
  { "id": [0-9]+, "key": { "name": "Lizard", "line": 20 } } (re)
  { "id": [0-9]+, "key": { "name": "Pet", "line": 10 } } (re)
  
  4 results, 4 facts, .*, .* bytes, .* compiled bytes (re)
