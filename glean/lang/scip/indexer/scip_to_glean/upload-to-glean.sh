#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.


set -Eeuox pipefail

glean --service glean.write.test delete --db aosp.oculus.14.java/localtest
glean --service glean.write.test create --db aosp.oculus.14.java/localtest
glean --service glean.write.test write --db aosp.oculus.14.java/localtest ~/fbsource/index.json
glean --service glean.write.test complete --db aosp.oculus.14.java/localtest
glean --service glean.write.test derive --db aosp.oculus.14.java/localtest \
    scip.LowerCaseDisplayNameSymbol \
    scip.DefinitionLocation \
    scip.ReferenceLocation \
    scip.SymbolDisplayName \
    scip.IsImplemented
glean --service glean.write.test finish --db aosp.oculus.14.java/localtest
glean --service glean.write.test stats --db aosp.oculus.14.java/localtest --per-predicate
