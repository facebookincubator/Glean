# set BUILD=buck2 to use buck2
BUILD ?= cabal

CABAL_BIN=cabal
PWD := $(shell pwd)

# There's a lot of parallelism in the schema-generated code
# If you have >=16G and >=4 cores, trying passing these:
#
# EXTRA_GHC_OPTS = '-j4 +RTS -A128m -n2m -RTS'
#
EXTRA_GHC_OPTS ?=

# Run recipes under bash so `set -o pipefail` works (see the test target).
SHELL := /bin/bash

CABAL = $(CABAL_BIN) --jobs --ghc-options='$(EXTRA_GHC_OPTS)' \
            -vnormal+nowrap --project-file=$(PWD)/cabal.project \
			$(CABAL_CONFIG_FLAGS) $(GETDEPS_CABAL_FLAGS)

BUILD_DIR = .build
CODEGEN_DIR = $(BUILD_DIR)/codegen

BYTECODE_GEN= \
	glean/hs/Glean/RTS/Bytecode/Gen/Instruction.hs \
	glean/hs/Glean/RTS/Bytecode/Gen/Issue.hs \
	glean/hs/Glean/RTS/Bytecode/Gen/Version.hs

BYTECODE_SRCS= \
	$(wildcard glean/bytecode/*/Glean/Bytecode/*/*.hs) \
	$(wildcard glean/bytecode/Glean/Bytecode/*.hs)

# Code generators. May be injected by external build systems if those are
# managing the build.
ifeq ($(BUILD),buck2)
GEN_SCHEMA = buck2 run glean/schema/gen:gen-schema --
else
GEN_SCHEMA = $(CABAL) run glean:gen-schema --
endif
GEN_BYTECODE = $(CABAL) run glean:gen-bytecode-hs --

all:: thrift $(BYTECODE_GEN) gen-schema thrift-schema-hs glean

.PHONY: cabal-update
cabal-update::
	$(CABAL) update

# Targets in this file invoke Cabal and hence can't be built in parallel
.NOTPARALLEL:

.PHONY: glean
glean::
	set -o pipefail; $(CABAL) build glean glean-server glean-hyperlink 2>&1 | grep -vF 'experimental feature (issue #5660)'

SCIP_TO_GLEAN_DIR = glean/lang/scip/indexer/scip_to_glean

.PHONY: scip-to-glean
scip-to-glean::
	cd $(SCIP_TO_GLEAN_DIR) && cargo build --release
	@echo "scip-to-glean binary built at $(SCIP_TO_GLEAN_DIR)/target/release/scip-to-glean"

.PHONY: gen-bytecode
gen-bytecode: $(BYTECODE_GEN)

# Note we don't rsync here because we have actual dependencies
$(BYTECODE_GEN) &: $(BYTECODE_SRCS)
	$(GEN_BYTECODE) --install_dir=glean/hs

.PHONY: test
test::
	set -o pipefail; $(CABAL) test glean:tests --test-show-details=failures 2>&1 | grep -vF 'experimental feature (issue #5660)'

SCHEMAS= \
	anglelang \
	buck \
	builtin \
	chef \
	code \
	code_anglelang \
	code_buck \
	code_chef \
	code_csharp \
	code_cxx \
	code_dataswarm \
	code_erlang \
	code_fbthrift \
	code_flow \
	code_graphql \
	code_hack \
	code_hs \
	code_java \
	code_kotlin \
	code_lsif \
	code_swift \
	codemarkup \
	codemarkup_anglelang \
	codemarkup_buck \
	codemarkup_chef \
	codemarkup_csharp \
	codemarkup_cxx \
	codemarkup_dataswarm \
	codemarkup_erlang \
	codemarkup_fbthrift \
	codemarkup_flow \
	codemarkup_graphql \
	codemarkup_hack \
	codemarkup_haskell \
	codemarkup_java \
	codemarkup_lsif \
	codemarkup_pp \
	codemarkup_python \
	codemarkup_scip \
	codemarkup_search \
	codemarkup_swift \
	codemarkup_types \
	code_pp \
	code_python \
	code_scip \
	csharp \
	cxx1 \
	dataswarm \
	digest \
	dyn \
	erlang \
	flow \
	gencode \
	glass \
	glean_test \
	graphql \
	hack \
	hs \
	java_alpha \
	javakotlin_alpha \
	kotlin_alpha \
	lsif \
	lsif_types \
	pp1 \
	python \
	scip \
	search_anglelang \
	search_chef \
	search_code \
	search_cxx \
	search_erlang \
	search_buck \
	search_hack \
	search_java \
	search_kind_cxx \
	search_hs \
	search_pp \
	src \
	symbolid_cxx \
	symbolid_java \
	symbolid_kotlin \
	sys \
	fbthrift \

.PHONY: thrift
thrift:: thrift-compiler thrift-hs

.PHONY: thrift-hs
thrift-hs:: thrift-hsthrift-hs thrift-glean-hs

.PHONY: thrift-compiler
# Allow injecting a prebuilt thrift compiler by setting THRIFT_COMPILE in
# environment (e.g. with Nix).
ifndef THRIFT_COMPILE
thrift-compiler::
	(cd hsthrift && make CABAL="$(CABAL)" compiler)
	$(eval THRIFT_COMPILE := $$(shell $$(CABAL) -v0 list-bin exe:thrift-compiler))
else
thrift-compiler::
	# no-op
endif

.PHONY: thrift-hsthrift-hs
ifndef THRIFT_COMPILE
thrift-hsthrift-hs::
	(cd hsthrift && make CABAL="$(CABAL)" thrift-hs)
else
thrift-hsthrift-hs::
	# no-op
endif

.PHONY: gen-schema
gen-schema ::
	rm -rf $(CODEGEN_DIR)/$@
	mkdir -p $(CODEGEN_DIR)/$@
	$(GEN_SCHEMA) \
		--install_dir $(CODEGEN_DIR)/$@ \
		--dir glean/schema/source \
		--thrift glean/schema \
		--hs glean/schema \
		--cpp glean/lang/clang/schema.h
	rsync -r --checksum $(CODEGEN_DIR)/$@/ .
	# we have to copy the generated C++ headers to a designated place for reasons
	mkdir -p glean/schema/cpp
	rsync $(CODEGEN_DIR)/gen-schema/glean/lang/clang/schema.h glean/schema/cpp/schema.h

THRIFT_GLEAN= \
	glean/github/if/fb303.thrift \
	glean/github/if/fb303_core.thrift \
	glean/if/facebook/auth.thrift \
	glean/if/glean.thrift \
	glean/config/recipes/recipes.thrift \
	glean/config/recipes/recipes.thrift \
	glean/config/server/server_config.thrift \
	glean/config/service.thrift \
	glean/config/client/client_config.thrift \
	thrift/annotation/cpp.thrift \
	thrift/annotation/hack.thrift \
	thrift/annotation/haskell.thrift \
	thrift/annotation/rust.thrift \
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
	done
	rsync -r --checksum $(CODEGEN_DIR)/$@/ .

# full build up to glass lib
.PHONY: glass-lib
glass-lib:: thrift gen-schema thrift-schema-hs thrift-glean-hs
	$(CABAL) build glass-lib

# short circuit target to avoid thrift regen
.PHONY: glass
glass::
	$(CABAL) build glass-server glass-democlient

.PHONY: glean-clang
glean-clang:: gen-schema glean glean/schema/cpp/schema.h
	$(CABAL) build glean-clang

.PHONY: glean-hie
glean-hie::
	$(CABAL) build hie-indexer

.PHONY: glean-lsp
glean-lsp::
	$(CABAL) build glean-lsp

define bash_macros
call_cabal() {
	$(CABAL) "$$@"
}
endef

$(BUILD_DIR)/current.sh: force
	$(file >$@,$(bash_macros))
	@:

# Dummy install rule to keep getdeps happy. TODO: actually install things
.PHONY: install
install::
	mkdir -p $(PREFIX)

.PHONY: setup-folly
setup-folly::
	$(MAKE) -C hsthrift setup-folly

.PHONY: setup-folly-version
setup-folly-version::
	$(MAKE) -C hsthrift setup-folly-version
