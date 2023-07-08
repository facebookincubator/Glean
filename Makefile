#
# This Makefile is quite primitive, it's just about enough to build
# everything for CI, but for development we'll want something better
# eventually.
#
# To build everything: type "make"
#
# To run the tests, type "make test"
#
# For development, use "make MODE=dev" for faster, unoptimised builds and
# "make MODE=opt" for slower, optimised builds.
#
# =====
# Modes
# =====
#
# By default, make builds everything via Cabal using standard Cabal directories.
# With MODE=dev|opt, make will use a build directory specific to that mode and
# separate from the default build directories and from other modes. This means
# that the individual modes don't interfere with each other and can be worked
# with without rebuilding everything. Typically, you want MODE=dev for
# development and MODE=opt for benchmarking.
#
# The dev and opt modes also build C++ libraries via make rather than Cabal as
# described below.
#
# =======================
# Cabal and C++ libraries
# =======================
#
# We have two modes of compiling Glean's internal C++ libraries, controlled by
# the CXX_MODE variable. These modes need slightly different glean.cabal files.
# Thus, we generate glean.cabal by running glean.cabal.in through m4 with
# appropriate macro definitions.
#
# The reason for this is that Cabal can't build .cpp files in parallel and also
# doesn't track include dependencies between different libraries with
# cxx-sources. However, Cabal is the only way to get things onto Hackage. Thus,
# we support building C++ libraries via Cabal but for development, building
# via make should be preferred.
#
# Building via make (CXX_MODE=make):
#   * Builds C++ libraries outside of Cabal
#   * Compiles C++ modules in parallel (about 2:30min faster for full build)
#   * Automatically tracks include dependencies
#
# Building via Cabal (otherwise):
#   * Might work on Hackage
#
# The C++ libraries are defined in mk/cxx.mk, see the docs there for adding,
# removing or modifying libraries.
#
# ===================
# Auto-generated code
# ===================
#
# Re-running "make" will regenerate a bunch of things. That's because
# the Makefile is dumb and doesn't know much about the dependencies of
# the generated files. To avoid recompiling any code if nothing changed,
# we generally write generated code into $(CODEGEN_DIR) and then
# rsync --checksum it to the actual source directory which preserves timestamps
# for files which haven't changed.
#

CABAL_BIN=cabal
PWD := $(shell /bin/pwd)

# There's a lot of parallelism in the schema-generated code
# If you have >=16G and >=4 cores, trying passing these:
#
# EXTRA_GHC_OPTS = '-j4 +RTS -A128m -n2m -RTS'
#
EXTRA_GHC_OPTS ?=

# Allow developers to locally override things
-include settings.mk

MODE ?= def

ifneq ($(MODE),def)
include mk/mode-$(MODE).mk
endif

CABAL = $(CABAL_BIN) --jobs --ghc-options='$(EXTRA_GHC_OPTS)' \
            -vnormal+nowrap --project-file=$(PWD)/cabal.project \
			$(CABAL_CONFIG_FLAGS)

BUILD_DIR = .build
MODE_DIR = $(BUILD_DIR)/$(MODE)
CODEGEN_DIR = $(MODE_DIR)/codegen
CXX_DIR = $(MODE_DIR)/cxx

BYTECODE_GEN= \
	glean/rts/bytecode/gen/evaluate.h \
	glean/rts/bytecode/gen/instruction.h \
	glean/hs/Glean/RTS/Bytecode/Gen/Instruction.hs \
	glean/hs/Glean/RTS/Bytecode/Gen/Issue.hs \
	glean/hs/Glean/RTS/Bytecode/Gen/Version.hs

BYTECODE_SRCS= \
	$(wildcard glean/bytecode/*/Glean/Bytecode/*/*.hs) \
	$(wildcard glean/bytecode/Glean/Bytecode/*.hs)

all:: thrift $(BYTECODE_GEN) gen-schema thrift-schema-hs glean

# Targets in this file invoke Cabal and hence can't be built in parallel
.NOTPARALLEL:

.PHONY: force
$(BUILD_DIR)/mode: force
	@mkdir -p $(@D)
	@(echo $(MODE) | cmp -s $@) || (echo $(MODE) > $@)

# We have to regenerate glean.cabal if the mode (and hence the path to defs.m4)
# changes even if the actual files are older.
glean.cabal: glean.cabal.in $(BUILD_DIR)/mode $(CXX_DIR)/defs.m4
	rm -f $@
	m4 -E -E -P $(CXX_DIR)/defs.m4 glean.cabal.in \
		| sed "/-- Copyright/a \\\n-- @""generated from glean.cabal.in\\n-- DO NO EDIT THIS FILE DIRECTLY" \
		> $@
	chmod guo-w $@

$(CXX_DIR)/defs.m4: force
	@$(MAKE) -f mk/cxx.mk --no-print-directory CXX_MODE=$(CXX_MODE) CXX_DIR=$(CXX_DIR) $@

.PHONY: cxx-libraries
cxx-libraries: gen-bytecode
	@$(MAKE) -f mk/cxx.mk --no-print-directory CXX_MODE=$(CXX_MODE) CXX_DIR=$(CXX_DIR) $@

cxx-test-%: force
	@$(MAKE) -f mk/cxx.mk --no-print-directory CXX_MODE=$(CXX_MODE) CXX_DIR=$(CXX_DIR) $@

.PHONY: glean
glean:: glean.cabal cxx-libraries
	$(CABAL) build glean glean-server glean-hyperlink

.PHONY: gen-bytecode
gen-bytecode: $(BYTECODE_GEN)

# Note we don't rsync here because we have actual dependencies
$(BYTECODE_GEN) &: $(BYTECODE_SRCS) glean.cabal
	$(CABAL) run gen-bytecode-cpp -- --install_dir=glean/rts
	$(CABAL) run gen-bytecode-hs -- --install_dir=glean/hs

.PHONY: test
test:: glean.cabal cxx-libraries glean-clang
	$(CABAL) test glean:tests

SCHEMAS= \
	buck \
	builtin \
	code \
	code_buck \
	code_cxx \
	code_erlang \
	code_flow \
	code_hack \
	code_hs \
	code_java \
	code_lsif \
	codemarkup \
	codemarkup_buck \
	codemarkup_cxx \
	codemarkup_erlang \
	codemarkup_flow \
	codemarkup_hack \
	codemarkup_haskell \
	codemarkup_java \
	codemarkup_lsif \
	codemarkup_pp \
	codemarkup_python \
	codemarkup_scip \
	codemarkup_search \
	codemarkup_thrift \
	codemarkup_types \
	code_pp \
	code_python \
	code_scip \
	code_thrift \
	cxx1 \
	docmarkup \
	dyn \
	erlang \
	flow \
	gencode \
	glean_test \
	graphql \
	hack \
	hs \
	java \
	java_alpha \
	javakotlin_alpha \
	lsif \
	lsif_types \
	pp1 \
	python \
	scip \
	search_code \
	search_cxx \
	search_erlang \
	search_buck \
	search_hack \
	search_java \
	search_hs \
	search_pp \
	src \
	symbolid_cxx \
	symbolid_java \
	sys \
	thrift \

.PHONY: thrift
thrift:: thrift-compiler thrift-hs

.PHONY: thrift-hs
thrift-hs:: thrift-hsthrift-hs thrift-glean-hs

.PHONY: thrift-compiler
thrift-compiler::
	(cd hsthrift && make CABAL="$(CABAL)" compiler)
	$(eval THRIFT_COMPILE := $$(shell $$(CABAL) -v0 list-bin exe:thrift-compiler))

.PHONY: thrift-hsthrift-hs
thrift-hsthrift-hs::
	(cd hsthrift && make CABAL="$(CABAL)" thrift-hs)

.PHONY: gen-schema
gen-schema :: glean.cabal cxx-libraries
	rm -rf $(CODEGEN_DIR)/$@
	mkdir -p $(CODEGEN_DIR)/$@
	$(CABAL) run glean:gen-schema -- \
		--install_dir $(CODEGEN_DIR)/$@ \
		--dir glean/schema/source \
		--thrift glean/schema \
		--hs glean/schema \
		--cpp glean/lang/clang/schema.h
	rsync -r --checksum $(CODEGEN_DIR)/$@/ .

THRIFT_GLEAN= \
	glean/github/if/fb303.thrift \
	glean/github/if/fb303_core.thrift \
	glean/if/glean.thrift \
	glean/config/recipes/recipes.thrift \
	glean/config/recipes/recipes.thrift \
	glean/config/server/server_config.thrift \
	glean/config/service.thrift \
	glean/config/client/client_config.thrift \
	thrift/annotation/cpp.thrift \
	thrift/annotation/scope.thrift \
	thrift/annotation/thrift.thrift

.PHONY: thrift-glean-hs
thrift-glean-hs: thrift-compiler
	rm -rf $(CODEGEN_DIR)/$@
	mkdir -p $(CODEGEN_DIR)/$@
	for f in $(THRIFT_GLEAN); do \
		$(THRIFT_COMPILE) --hs $$f -o $(CODEGEN_DIR)/$@/$$(dirname $$f); \
	done
	# internal goes in a subdir, so do it separately
	$(THRIFT_COMPILE) --hs glean/if/internal.thrift \
		-o $(CODEGEN_DIR)/$@/glean/if/internal
	# index goes in a subdir, so do it separately
	$(THRIFT_COMPILE) --hs glean/if/index.thrift \
		-o $(CODEGEN_DIR)/$@/glean/if/index
	# glass goes in a subdir, so do it separately
	$(THRIFT_COMPILE) --hs glean/glass/if/glass.thrift \
		-o $(CODEGEN_DIR)/$@/glean/glass/if/glass
	rsync -r --checksum $(CODEGEN_DIR)/$@/ .

.PHONY: thrift-schema-hs
thrift-schema-hs: thrift-compiler
	rm -rf $(CODEGEN_DIR)/$@
	mkdir -p $(CODEGEN_DIR)/$@
	for s in $(SCHEMAS); do \
		$(THRIFT_COMPILE) --hs \
			glean/schema/thrift/$$s.thrift \
			-o $(CODEGEN_DIR)/$@/glean/schema/thrift; \
		$(THRIFT_COMPILE) --hs \
			glean/schema/thrift/query/$$s.thrift \
			-o $(CODEGEN_DIR)/$@/glean/schema/thrift/query; \
	done
	# This depends on the schema .thrift files:
	$(THRIFT_COMPILE) --hs glean/if/search.thrift \
		-o $(CODEGEN_DIR)/$@/glean/if/search
	rsync -r --checksum $(CODEGEN_DIR)/$@/ .

# full build up to glass lib
.PHONY: glass-lib
glass-lib:: thrift gen-schema thrift-schema-hs thrift-glean-hs glean.cabal cxx-libraries
	$(CABAL) build glass-lib

# short circuit target to avoid thrift regen
.PHONY: glass
glass::
	$(CABAL) build glass-server glass-democlient

.PHONY: glean-clang
glean-clang:: gen-schema glean glean.cabal cxx-libraries
	$(CABAL) build glean-clang

.PHONY: glean-hiedb
glean-hiedb:: glean.cabal cxx-libraries
	$(CABAL) build hiedb-indexer

define bash_macros
call_cabal() {
	$(CABAL) "$$@"
}
endef

$(BUILD_DIR)/current.sh: force
	$(file >$@,$(bash_macros))
	@:
