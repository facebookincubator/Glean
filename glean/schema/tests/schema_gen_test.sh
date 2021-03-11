#! /bin/bash

"$1" -i "$2" --thrift /dev/stdout | diff -c ./glean/schema/schema.thrift -
