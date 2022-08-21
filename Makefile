#
# This Makefile is quite primitive, it's just about enough to build
# everything for CI, but for development we'll want something better
# eventually.
#
# To build everything: type "make"
#
# To run the tests, type "make test"
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

CABAL = $(CABAL_BIN) --jobs --ghc-options='$(EXTRA_GHC_OPTS)' \
            -vnormal+nowrap --project-file=$(PWD)/cabal.project

CODEGEN_DIR = .build/codegen

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

.PHONY: glean
glean::
	$(CABAL) build glean glean-server glean-hyperlink

# Note we don't rsync here because we have actual dependencies
$(BYTECODE_GEN) &: $(BYTECODE_SRCS)
	$(CABAL) run gen-bytecode-cpp -- --install_dir=glean/rts
	$(CABAL) run gen-bytecode-hs -- --install_dir=glean/hs

.PHONY: test
test::
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
	codemarkup_lsif \
	codemarkup_pp \
	codemarkup_python \
	codemarkup_rust \
	codemarkup_search \
	codemarkup_thrift \
	codemarkup_types \
	code_pp \
	code_python \
	code_rust \
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
	lsif \
	pp1 \
	python \
	rust \
	search_code \
	search_cxx \
	search_erlang \
	search_buck \
	search_hack \
	search_hs \
	search_pp \
	src \
	sys \
	thrift \

.PHONY: thrift
thrift:: thrift-cpp thrift-compiler thrift-hs

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
gen-schema ::
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
	glean/config/client/client_config.thrift

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

THRIFT_CPP= \
	glean/config/recipes/recipes.thrift \
	glean/config/server/server_config.thrift \
	glean/if/glean.thrift \
	glean/if/internal.thrift \
	glean/github/if/fb303_core.thrift \
	glean/github/if/fb303.thrift

.PHONY: thrift-cpp
thrift-cpp: thrift-hsthrift-cpp
	rm -rf $(CODEGEN_DIR)/$@
	for f in $(THRIFT_CPP); do \
		mkdir -p $(CODEGEN_DIR)/$@/$$(dirname $$f) ;\
		thrift1 -I . --gen mstch_cpp2 \
			-o $(CODEGEN_DIR)/$@/$$(dirname $$f) $$f; \
	done
	rsync -r --checksum $(CODEGEN_DIR)/$@/ .

.PHONY: thrift-hsthrift-cpp
thrift-hsthrift-cpp::
	(cd hsthrift && make CABAL="$(CABAL)" thrift-cpp)

# full build up to glass lib
.PHONY: glass-lib
glass-lib:: thrift $(BYTECODE_GEN) gen-schema thrift-schema-hs thrift-glean-hs
	$(CABAL) build glass-lib

# short circuit target to avoid thrift regen
.PHONY: glass
glass::
	$(CABAL) build glass-server glass-democlient

.PHONY: glean-clang
glean-clang:: gen-schema glean
	$(CABAL) build glean-clang

.PHONY: glean-hiedb
glean-hiedb::
	$(CABAL) build hiedb-indexer
