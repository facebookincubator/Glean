#! /bin/bash

"$1" --cpp | diff ./glean/lang/clang/schema.h -
