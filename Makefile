CABAL_BIN=cabal
PWD := $(shell /bin/pwd)
CABAL = $(CABAL_BIN) --project-file=$(PWD)/cabal.project

THRIFT_COMPILE = $(CABAL) new-run exe:thrift-compiler --

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
	$(CABAL) build glean glean-shell glean-server glean-hyperlink

$(BYTECODE_GEN) &: $(BYTECODE_SRCS)
	$(CABAL) run gen-bytecode-cpp -- --install_dir=glean/rts
	$(CABAL) run gen-bytecode-hs -- --install_dir=glean/hs

.PHONY: test
test::
	$(CABAL) new-test glean:tests

SCHEMAS= \
	buck \
	builtin \
	code_cxx \
	code_flow \
	code_hack \
	code_hs \
	code_java \
	codemarkup \
	code_python \
	code \
	cxx1 \
	flow \
	glean_test \
	graphql \
	hack \
	hs \
	java \
	pp1 \
	python \
	search_cxx \
	search_hack \
	search_pp \
	src \
	sys

.PHONY: thrift
thrift:: thrift-cpp thrift-hs

.PHONY: thrift-hs
thrift-hs:: thrift-hsthrift-hs thrift-glean-hs

.PHONY: thrift-hsthrift-hs
thrift-hsthrift-hs ::
	(cd hsthrift && make CABAL="$(CABAL)" thrift-hs)

.PHONY: gen-schema
gen-schema ::
	$(CABAL) run glean:gen-schema -- \
		--dir glean/schema/source \
		--thrift glean/schema \
		--hs glean/schema \

.PHONY: thrift-glean-hs
thrift-glean-hs ::
	$(THRIFT_COMPILE) --hs glean/github/if/fb303.thrift \
		-o glean/github/if
	$(THRIFT_COMPILE) --hs glean/github/if/fb303_core.thrift \
		-o glean/github/if
	$(THRIFT_COMPILE) --hs glean/if/glean.thrift \
		-o glean/if
	$(THRIFT_COMPILE) --hs glean/if/internal.thrift \
		-o glean/if/internal
	$(THRIFT_COMPILE) --hs \
		glean/config/recipes/recipes.thrift \
		-o glean/config/recipes
	$(THRIFT_COMPILE) --hs \
		glean/config/server/server_config.thrift \
		-o glean/config/server
	$(THRIFT_COMPILE) --hs \
		glean/config/service.thrift \
		-o glean/config
	$(THRIFT_COMPILE) --hs \
		glean/config/client/client_config.thrift \
		-o glean/config/client

.PHONY: thrift-schema-hs
thrift-schema-hs::
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

.PHONY: thrift-cpp
thrift-cpp::
	(cd hsthrift && make CABAL="$(CABAL)" thrift-cpp)
	thrift1 -I . --gen mstch_cpp2 \
		-o glean/config/recipes \
		glean/config/recipes/recipes.thrift
	thrift1 -I . --gen mstch_cpp2 \
		-o glean/config/server \
		glean/config/server/server_config.thrift
	thrift1 -I . -I .. --gen mstch_cpp2 \
		-o glean/if \
		glean/if/glean.thrift
	thrift1 -I . -I .. --gen mstch_cpp2 \
		-o glean/if \
		glean/if/internal.thrift
	thrift1 -I . -I .. --gen mstch_cpp2 \
		-o glean/github/if \
		glean/github/if/fb303_core.thrift
	thrift1 -I . -I .. --gen mstch_cpp2 \
		-o glean/github/if \
		glean/github/if/fb303.thrift
