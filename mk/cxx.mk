# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

# C++ library definitions
#
# To define a C++ library glean_cpp_lib, add
#
# CXX_SOURCES_glean_cpp_lib = ...
#
# Additionaly, we support:
#
# CXX_FLAGS_glean_cpp_lib = ... - sets additional C++ flags for the library

CXX_SOURCES_glean_cpp_if_fb303 = \
    glean/github/if/gen-cpp2/fb303_core_types.cpp \
    glean/github/if/gen-cpp2/fb303_core_data.cpp \
    glean/github/if/gen-cpp2/fb303_types.cpp \
    glean/github/if/gen-cpp2/fb303_data.cpp

CXX_SOURCES_glean_cpp_config = \
    glean/config/recipes/gen-cpp2/recipes_types.cpp \
    glean/config/recipes/gen-cpp2/recipes_data.cpp

CXX_SOURCES_glean_cpp_if = \
    glean/if/gen-cpp2/glean_types.cpp \
    glean/if/gen-cpp2/glean_data.cpp

CXX_SOURCES_glean_cpp_if_internal = \
    glean/if/gen-cpp2/internal_types.cpp \
    glean/if/gen-cpp2/internal_data.cpp

CXX_SOURCES_glean_cpp_rts = \
    glean/rts/binary.cpp \
    glean/rts/cache.cpp \
    glean/rts/define.cpp \
    glean/rts/error.cpp \
    glean/rts/fact.cpp \
    glean/rts/factset.cpp \
    glean/rts/ffi.cpp \
    glean/rts/inventory.cpp \
    glean/rts/json.cpp \
    glean/rts/lookup.cpp \
    glean/rts/nat.cpp \
    glean/rts/ownership.cpp \
    glean/rts/ownership/derived.cpp \
    glean/rts/ownership/setu32.cpp \
    glean/rts/ownership/slice.cpp \
    glean/rts/ownership/uset.cpp \
    glean/rts/prim.cpp \
    glean/rts/query.cpp \
    glean/rts/sanity.cpp \
    glean/rts/string.cpp \
    glean/rts/substitution.cpp \
    glean/rts/thrift.cpp \
    glean/rts/timer.cpp \
    glean/rts/validate.cpp \
    glean/rts/bytecode/subroutine.cpp
CXX_FLAGS_glean_cpp_rts = -DOSS=1

CXX_SOURCES_glean_cpp_rocksdb_stats = \
    glean/rocksdb/stats.cpp

CXX_SOURCES_glean_cpp_rocksdb = \
    glean/rocksdb/ffi.cpp \
    glean/rocksdb/rocksdb.cpp
# -fno-rtti is needed because RocksDB is compiled with it, and we
# get linker errors for references to missing typeinfo symbols if
# we don't.
CXX_FLAGS_glean_cpp_rocksdb = -fno-rtti -DOSS=1

CXX_SOURCES_glean_cpp_client = \
    glean/cpp/sender.cpp \
    glean/cpp/glean.cpp \
    glean/interprocess/cpp/worklist.cpp \
    glean/interprocess/cpp/counters.cpp
CXX_FLAGS_glean_cpp_client = -DOSS=1

CXX_GTEST_SOURCES_BinaryTest = \
    glean/rts/tests/BinaryTest.cpp
CXX_GTEST_LIBS_BinaryTest = glean_cpp_rts
CXX_GTEST_FLAGS_BinaryTest = -DOSS=1

# End of C++ library definitions

# Determine all C++ libraries
CXX_LIBRARIES = $(subst CXX_SOURCES_,,$(filter CXX_SOURCES_%, $(.VARIABLES)))

CXX_GTESTS = $(subst CXX_GTEST_SOURCES_,,$(filter CXX_GTEST_SOURCES_%, $(.VARIABLES)))

.PHONY: cxx-libraries
cxx-libraries:
	@:

# Include the right settings
#
# The included makefile is supposed to add dependencies to cxx-libraries as
# necessary and also to define a variable CXX_DEFS_M4 whose value is a series
# of m4 defines for the following macros:
#
# CXX_EXTRA_SOURCE_FILES - extra-source-files: ...
#   Additional files to track for Cabal (typically .a files)
#   
# CXX_EXTRA_LIB_DIRS - extra-lib-dirs: ...
#   Additional library directories, MUST be absolute (Cabal doesn't support
#   relative ones)
#
# CXX_LIB_xxx
#   Dependencies for library xxx - either cxx-sources and cxx-options (if
#   compiled via Cabal) or extra-libraries (if compiled via make)
# 
ifeq ($(CXX_MODE),make)
include mk/cxx-make.mk
else
include mk/cxx-cabal.mk
endif

# Generate defs.m4 which is used by glean.cabal.in
.PHONY: force

# Make sure we keep the timestamp if the contents hasn't changed
$(CXX_DIR)/defs.m4: force | $(CXX_DIR)
	$(file >$@.new,$(CXX_DEFS_M4))
	@cmp -s $@ $@.new || mv $@.new $@
	@rm -rf $@.new

# Has to be a separate target because we can have shell commands before
# $(file) in the rule above.
$(CXX_DIR):
	@mkdir -p $@
