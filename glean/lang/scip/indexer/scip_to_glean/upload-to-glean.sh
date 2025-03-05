#!/bin/bash
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

set -Eeuox pipefail

glean --service glean.write.test delete --db aosp.oculus.14.java/localtest
glean --service glean.write.test create --db aosp.oculus.14.java/localtest
glean --service glean.write.test write --db aosp.oculus.14.java/localtest ~/fbsource/index.json
glean --service glean.write.test complete --db aosp.oculus.14.java/localtest
glean --service glean.write.test derive --db aosp.oculus.14.java/localtest scip.DefinitionLocation scip.ReferenceLocation
glean --service glean.write.test finish --db aosp.oculus.14.java/localtest
glean --service glean.write.test stats --db aosp.oculus.14.java/localtest --per-predicate
