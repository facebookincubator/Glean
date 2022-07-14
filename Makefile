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
# the generated files. However, re-running make shouldn't actually
# recompile any code if nothing changed; the Thrift compiler won't
# overwrite files that haven't changed, so Cabal won't have to rebuild
# anything.
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

THRIFT_COMPILE := $(shell $(CABAL) -v0 list-bin exe:thrift-compiler)

BYTECODE_GEN= \
	glean/rts/bytecode/gen/evaluate.h \
	glean/rts/bytecode/gen/instruction.h \
	glean/hs/Glean/Bytecode/Gen/Instruction.hs \
	glean/hs/Glean/Bytecode/Gen/Issue.hs \
	glean/hs/Glean/Bytecode/Gen/Version.hs

BYTECODE_SRCS= \
	$(wildcard glean/bytecode/*/Glean/Bytecode/*/*.hs) \
	$(wildcard glean/bytecode/Glean/Bytecode/*.hs)

all:: thrift $(BYTECODE_GEN) gen-schema thrift-schema-hs glean

.PHONY: glean
glean::
	$(CABAL) build glean glean-server glean-hyperlink

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

.PHONY: thrift-hsthrift-hs
thrift-hsthrift-hs::
	(cd hsthrift && make CABAL="$(CABAL)" thrift-hs)

.PHONY: gen-schema
gen-schema ::
	$(CABAL) run glean:gen-schema -- \
		--dir glean/schema/source \
		--thrift glean/schema \
		--hs glean/schema \
		--cpp glean/lang/clang/schema.h

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
	for f in $(THRIFT_GLEAN); do \
		$(THRIFT_COMPILE) --hs $$f -o $$(dirname $$f); \
	done
	# internal goes in a subdir, so do it separately
	$(THRIFT_COMPILE) --hs glean/if/internal.thrift \
		-o glean/if/internal
	# index goes in a subdir, so do it separately
	$(THRIFT_COMPILE) --hs glean/if/index.thrift \
		-o glean/if/index
	# glass goes in a subdir, so do it separately
	$(THRIFT_COMPILE) --hs glean/glass/if/glass.thrift \
		-o glean/glass/if/glass


.PHONY: thrift-schema-hs
thrift-schema-hs: thrift-compiler
	for s in $(SCHEMAS); do \
		$(THRIFT_COMPILE) --hs \
			glean/schema/thrift/$$s.thrift \
			-o glean/schema/thrift; \
		$(THRIFT_COMPILE) --hs \
			glean/schema/thrift/query/$$s.thrift \
			-o glean/schema/thrift/query; \
	done
	# This depends on the schema .thrift files:
	$(THRIFT_COMPILE) --hs glean/if/search.thrift \
		-o glean/if/search

THRIFT_CPP= \
	glean/config/recipes/recipes.thrift \
	glean/config/server/server_config.thrift \
	glean/if/glean.thrift \
	glean/if/internal.thrift \
	glean/github/if/fb303_core.thrift \
	glean/github/if/fb303.thrift

.PHONY: thrift-cpp
thrift-cpp: thrift-hsthrift-cpp
	for f in $(THRIFT_CPP); do \
		thrift1 -I . --gen mstch_cpp2 -o $$(dirname $$f) $$f; \
	done

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
