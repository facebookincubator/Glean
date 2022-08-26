# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.

#
# Building C++ libraries via make
#

FOLLY_INCLUDES := $(shell pkg-config --cflags-only-I libfolly)
ARCH := $(firstword $(subst -, ,$(shell $(CC) -dumpmachine)))

# TODO: This is duplicated in glean.cabal.in
CXXFLAGS += -I. -Ihsthrift/common/util $(FOLLY_INCLUDES) -std=c++17
ifeq ($(ARCH), x86_64)
CXXFLAGS += -march=haswell
endif

ifndef CXX_DIR
$(error CXX_DIR is not set)
endif

# Target directory for .a files
CXX_LIB_DIR = $(CXX_DIR)/lib

# Target directory for .o files
CXX_OBJ_DIR = $(CXX_DIR)/obj

# Actually build the libraries - the individual targets are defined via
# define_library below
cxx-libraries: $(CXX_LIBRARIES)

# For each library xxx, we define some variables and a target xxx which builds
# the actual library. We also track include dependencies via the usual
# mechanism.
#
# Variables:
#   CXX_OBJECTS_xxx - list of .o files
#   CXX_LIB_xxx - path to .a file
#
# TODO: We compile everything with -fPIC so that it can be used in both static
# and dynamic builds. We might want to build two versions eventually. Note that
# we probably *don't* want to build actual shared libraries because then we'd
# have to fiddle with LD_LIBRARY_PATH to actual run things.
#
define define_library
 CXX_OBJECTS_$1 = $$(addprefix $$(CXX_OBJ_DIR)/,$$(CXX_SOURCES_$1:.cpp=.o))
 CXX_LIB_$1 = $$(CXX_LIB_DIR)/lib$1.a
 .PHONY: $1
 $1:: $$(CXX_LIB_$1)

 $$(CXX_LIB_$1): $$(CXX_OBJECTS_$1)
	@mkdir -p $$(@D)
	$$(AR) rcs $$@ $$^

 $$(CXX_OBJECTS_$1): $(CXX_OBJ_DIR)/%.o: %.cpp
	@mkdir -p $$(@D)
	$$(CXX) -o $$@ $$(CXXFLAGS) $$(CXX_FLAGS_$1) -fPIC -MMD -MP -c $$<

 -include $$(CXX_OBJECTS_$1:.o=.d)
endef

# Call define_library for each library
$(foreach lib, $(CXX_LIBRARIES), $(eval $(call define_library,$(lib))))

# Track .a files via extra-source-files - a terrible hack but seems to be the
# only way to recompile things when the libraries change, see
# https://github.com/haskell/cabal/issues/4746.
define CXX_DEFS_M4
m4_divert(-1)
m4_define(`CXX_EXTRA_SOURCE_FILES', extra-source-files:$(foreach lib, $(CXX_LIBRARIES),
    $(CXX_LIB_$(lib))))
m4_define(`CXX_EXTRA_LIB_DIRS', extra-lib-dirs: $(PWD)/$(CXX_LIB_DIR)))
$(foreach lib, $(CXX_LIBRARIES),
m4_define(`CXX_LIB_$(lib)', extra-libraries: $(lib)))
m4_divert(0)m4_dnl
endef
