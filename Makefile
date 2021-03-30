CABAL_BIN=cabal
PWD := $(shell /bin/pwd)
CABAL = $(CABAL_BIN) --project-file=$(PWD)/cabal.project

THRIFT_COMPILE = $(CABAL) new-run exe:thrift-compiler --

glean::
	$(CABAL) new-build glean

gen-bytecode::
	$(CABAL) new-run gen-bytecode-cpp -- --install_dir=glean/rts
	$(CABAL) new-run gen-bytecode-hs -- --install_dir=glean/hs

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

thrift:: thrift-cpp thrift-hs

thrift-hs:: thrift-hsthrift-hs thrift-glean-hs

thrift-hsthrift-hs ::
	(cd hsthrift && make CABAL="$(CABAL)" thrift-hs)

gen-schema ::
	$(CABAL) run glean:gen-schema -- \
		--dir glean/schema/source \
		--thrift glean/schema \
		--hs glean/schema \

thrift-glean-hs ::
	$(THRIFT_COMPILE) --hs glean/github/if/fb303.thrift \
		-o glean/github/if
	$(THRIFT_COMPILE) --hs glean/github/if/fb303_core.thrift \
		-o glean/github/if
	$(THRIFT_COMPILE) --hs glean/if/glean.thrift \
		-o glean/if
	$(THRIFT_COMPILE) --hs glean/if/internal.thrift \
		-o glean/if/internal
	$(THRIFT_COMPILE) --hs glean/if/search.thrift \
		-o glean/if/search
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

thrift-schema-hs::
	for s in $(SCHEMAS); do \
		$(THRIFT_COMPILE) --hs \
			glean/schema/thrift/$$s.thrift \
			-o glean/schema/thrift; \
		$(THRIFT_COMPILE) --hs \
			glean/schema/thrift/query/$$s.thrift \
			-o glean/schema/thrift/query; \
	done

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
