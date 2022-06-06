# Copyright (c) Facebook, Inc. and its affiliates.

  $ source "$TESTDIR/setup.sh"
  Creating DB using handle fbcode:glean/tools/gleancli:glean@.* (re)

  $ function query { "$GLEAN" --service "::1:$PORT" shell --db "$DB" "$(echo -e $1)" ; }

  $ query ":help"
  [>] :help (re)
  Glean Shell.+ (re)
  
  Commands:
    :database [<db>]                         Use database <db>
    :index <lang> <dir>                      Index source files in <dir> and create a database.
    :list [<db>]                             List available databases which match <db>
    :list-all [<db>]                         List available databases and restorable backups which match <db>
    :debug off|[-]ir|[-]bytecode|all         Enable/disable query debugging options
    :describe [<db>]                         Like :list, but show more details
    :describe-all [<db>]                     Like :list-all, but show more details
    :mode [json|angle]                       Select mode for query syntax and results
    :schema [predicate|type]                 Show schema for the given predicate or type
    :edit                                    Edit a query in an external editor. Set the EDITOR environment variable to choose an editor
    :limit <n>                               Set limit on the number of query results
    :load (<file> | <db>/<hash> <file> ...)  Create a DB from file(s) of JSON facts
    :timeout off|<n>                         Set the query time budget
    :expand off|on                           Recursively expand nested facts in the response
    :pager off|on                            Enable/disable result paging
    :count <query>                           Show only a count of query results, not the results themselves
    :more                                    Fetch more results from the previous query
    :profile [off|summary|full]              Show query profiling information
    :reload                                  Reload the schema (when using --schema)
    :statistics [-s] [<predicate>]           Show statistics for the current database, sorted by decreasing size when using -s
    :quit                                    Exit the shell
  
  Queries (angle mode):
    {1234}                    Look up a fact by its Id
    <predicate> <pat>         Query a predicate for facts matching <pat>
  
  Pattern syntax (angle mode):
    1234                     :: byte or nat
    "abc"                    :: string
    "abc"..                  :: string prefix match
    true|false               :: bool
    [ val1, val2]            :: array(T)
    [ val1, val2, ..]        :: array(T) prefix
    { field = val, ... }     :: record(fields), omitted fields are wild
    { field = val }          :: sum(fields)
  
  Please consult the documentation for the full query syntax.
  
  Examples:
    {1234}                                   fetch a fact by its Id
    pp1.Define _                             all the pp1.Define facts
    pp1.Define { macro = "NULL" }            every #define of NULL






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
  not in scope: A
       
  1 |  A B
       ^
  [1]


  $ query "A B"
  [>] A B (re)
  not in scope: A
       
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








Recursive expansion is on by default
  $ "$GLEAN" --service "::1:$PORT" shell --db "$DB" ":limit 1" "example.Parent _" | head -n -4
  [>] :limit 1 (re)
  [>] example.Parent _ (re)
  { "id": 1028, "key": { "child": { "id": 1025, "key": { "name": "Lizard", "line": 20 } }, "parent": { "id": 1024, "key": { "name": "Pet", "line": 10 } } } }

Setting `:expand off` disables recursive expansion
  $ "$GLEAN" --service "::1:$PORT" shell --db "$DB" ":limit 1" ":expand off" "example.Parent _ " | head -n -4
  [>] :limit 1 (re)
  [>] :expand off (re)
  [>] example.+ (re)
  { "id": 1028, "key": { "child": { "id": 1025 }, "parent": { "id": 1024 } } }

  $ query "!example.Parent _ " | head -n -4
  [>] !example.Parent _ (re)
  WARNING: Deprecated syntax. '!' at the start of a line to recursively expand facts is deprecated.
  Fact expansion is now enabled by default.
  Use ':expand off' to disable it.
  { "id": 1028, "key": { "child": { "id": 1025, "key": { "name": "Lizard", "line": 20 } }, "parent": { "id": 1024, "key": { "name": "Pet", "line": 10 } } } }

  $ query ":expand"
  [>] :expand (re)
  result expansion is on

  $ query ":expand what"
  [>] :expand what (re)
  glean: syntax: :expand [off|on]
  [1]
