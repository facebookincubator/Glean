  $ source "$TESTDIR/setup.sh"
  Creating DB using handle fbcode:glean/tools/gleancli:glean@.* (re)

  $ function query { echo $1 | "$SHELL" --service "::1:$PORT" --db "$DB" | tail -n +3 | tail -c +10 | head -n -1; }

  $ query "example.Class _"
  { "id": [0-9]+, "key": { "name": "Fish", "line": 30 } } (re)
  { "id": [0-9]+, "key": { "name": "Goldfish", "line": 40 } } (re)
  { "id": [0-9]+, "key": { "name": "Lizard", "line": 20 } } (re)
  { "id": [0-9]+, "key": { "name": "Pet", "line": 10 } } (re)
  
  4 results, 4 facts, .*, .* bytes, .* compiled bytes (re)

  $ query "{ wrong = what } : string"
  line 1, column 1
  type error in pattern
      pattern: {wrong = what}
      expected type: string

  $ query "_"
  line 1, column 1
  can't infer the type of: _
      try adding a type annotation like (_ : T)
      or reverse the statement (Q = P instead of P = Q)

  $ query "A -> B"
  line 1, column 1
  a key/value pattern (X -> Y) cannot be used here

  $ query "A -> B"
  line 1, column 1
  a key/value pattern (X -> Y) cannot be used here

  $ query "A B"
  line 1, column 1
  unknown type or predicate while inferring application: A

  $ query "A B"
  line 1, column 1
  unknown type or predicate while inferring application: A

  $ query "B = 1; 1 = B;"
  line 1, column 8
  the last statement should be an expression: B = 1; 1 = B

  $ query "A = 1; B = A[..];"
  line 1, column 12
  type error in array element generator:
      expression: A
      does not have an array type

  $ query "A = \"a\"; B = A : nat"
  line 1, column 14
  type mismatch for variable A
      type of variable: string
      expected type: nat

  $ query "{ w = A } : { n : nat | s : nat };"
  line 1, column 7
  unknown alt: w
      pattern: A
      expected type: { n : nat | s : nat | }

  $ query "{} : { n : nat | s : nat };"
  line 1, column 1
  matching on a sum type should have the form { field = pattern }
      pattern: {}
      expected type: { n : nat | s : nat | }

  $ query "A; A"
  line 1, column 1
  variable has unknown type: A
      Perhaps you mistyped the variable name?
      If not, try adding a type annotation like: (A : T)
      or reverse the statement (Q = P instead of P = Q)

  $ query "a = 2"
  line 1, column 1
  variable does not begin with an upper-case letter: a
