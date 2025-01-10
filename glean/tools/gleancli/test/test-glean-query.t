  $ source "$TESTDIR/setup.sh"

  $ glean create --finish --db abc/0 --schema $EXAMPLE  "$EXAMPLE/facts.glean"
  Creating DB.* (re)
  Wrote.* (re)

  $ glean --schema $EXAMPLE query --db-name abc 'example.Class {name="Pet"}'
  {"id":[0-9]*,"key":{"name":"\w+","line":[0-9]+}} (re)

  $ glean --schema $EXAMPLE query --db-name abc 'example.Class {name="Pet"}' --stats "glean_stats_output.txt"
  {"id":[0-9]*,"key":{"name":"\w+","line":[0-9]+}} (re)

  $ cat "glean_stats_output.txt"
  {"num_facts":1,"elapsed_ns":[0-9]*,"allocated_bytes":[0-9]*,"compile_time_ns":[0-9]*,"bytecode_size":[0-9]*,"execute_time_ns":[0-9]*,"result_count":1,"codegen_time_ns":[0-9]*,"full_scans":\[],"result_bytes":[0-9]*} (re)

  $ glean --schema $EXAMPLE query --db-name abc 'example.Class {name="Pet"}' --stats "glean_profile_output.txt" --profile
  {"id":[0-9]*,"key":{"name":"\w+","line":[0-9]+}} (re)

  $ cat "glean_profile_output.txt"
  {"num_facts":1,"elapsed_ns":[0-9]*,"allocated_bytes":[0-9]*,"facts_searched":{"[0-9]*":1},"compile_time_ns":[0-9]*,"bytecode_size":[0-9]*,"execute_time_ns":[0-9]*,"result_count":1,"codegen_time_ns":[0-9]*,"full_scans":\[],"result_bytes":[0-9]*} (re)
