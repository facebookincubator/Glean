# Copyright (c) Facebook, Inc. and its affiliates.

  $ source "$TESTDIR/setup.sh"
  Creating DB using handle fbcode:glean/tools/gleancli:glean@.* (re)

  $ function query { "$SHELL" --service "::1:$PORT" --db "$DB" "$(echo -e $1)" ; }

  $ query "example.Class _"
  [>] example.Class _ (re)
  { "id": [0-9]+, "key": { "name": "Fish", "line": 30 } } (re)
  { "id": [0-9]+, "key": { "name": "Goldfish", "line": 40 } } (re)
  { "id": [0-9]+, "key": { "name": "Lizard", "line": 20 } } (re)
  { "id": [0-9]+, "key": { "name": "Pet", "line": 10 } } (re)
  
  4 results, 4 facts, .*, .* bytes, .* compiled bytes (re)

  $ query "{ wrong = what } : string"
  [>] { wrong = what } : string (re)
  type error in pattern
      pattern: {wrong = what}
      expected type: string
       
  1 |  { wrong = what } : string
       ^^^^^^^^^^^^^^^^
  [1]

  $ query "_"
  [>] _ (re)
  can't infer the type of: _
      try adding a type annotation like (_ : T)
      or reverse the statement (Q = P instead of P = Q)
       
  1 |  _
       ^
  [1]

  $ query "A -> B"
  [>] A -> B (re)
  a key/value pattern (X -> Y) cannot be used here
       
  1 |  A -> B
       ^^^^^^
  [1]

  $ query "A -> B"
  [>] A -> B (re)
  a key/value pattern (X -> Y) cannot be used here
       
  1 |  A -> B
       ^^^^^^
  [1]

  $ query "A B"
  [>] A B (re)
  unknown type or predicate while inferring application: A
       
  1 |  A B
       ^
  [1]

  $ query "A B"
  [>] A B (re)
  unknown type or predicate while inferring application: A
       
  1 |  A B
       ^
  [1]

  $ query "B = 1; 1 = B"
  [>] B = 1; 1 = B (re)
  the last statement should be an expression: B = 1; 1 = B
       
  1 |  B = 1; 1 = B
              ^
  [1]

  $ query "A = 1; B = A[..]"
  [>] A = 1; B = A[[]..[]] (re)
  type error in array element generator:
      expression: A
      does not have an array type
       
  1 |  A = 1; B = A[..]
                  ^^^^^
  [1]

  $ query "A = \"a\"; B = A : nat"
  [>] A = \"a\"; B = A : nat (re)
  type mismatch for variable A
      type of variable: string
      expected type: nat
       
  1 |  A = "a"; B = A : nat
                    ^
  [1]

  $ query "{ w = A } : { n : nat | s : nat }"
  [>] { w = A } : { n : nat | s : nat } (re)
  unknown alt: w
      pattern: A
      expected type: { n : nat | s : nat | }
       
  1 |  { w = A } : { n : nat | s : nat }
             ^
  [1]

  $ query "{} : { n : nat | s : nat }"
  [>] {} : { n : nat | s : nat } (re)
  matching on a sum type should have the form { field = pattern }
      pattern: {}
      expected type: { n : nat | s : nat | }
       
  1 |  {} : { n : nat | s : nat }
       ^^
  [1]

  $ query "A; A"
  [>] A; A (re)
  variable has unknown type: A
      Perhaps you mistyped the variable name?
      If not, try adding a type annotation like: (A : T)
      or reverse the statement (Q = P instead of P = Q)
       
  1 |  A; A
       ^
  [1]

  $ query "a = 2"
  [>] a = 2 (re)
  variable does not begin with an upper-case letter: a
       
  1 |  a = 2
       ^
  [1]

  $ query "A\n  where\n\n\n\n\n\n A = {\n what = "what"\n }"
  [>] A (re)
   where
  
  
  
  
  
   A = {
   what = what
   }
  can't infer the type of: {what = what}
      try adding a type annotation like ({what = what} : T)
      or reverse the statement (Q = P instead of P = Q)
        
   7 |  
   8 |>  A = {
   9 |>  what = what
  10 |>  }
        
  [1]
