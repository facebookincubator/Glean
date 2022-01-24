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
CABAL_PROJECT := --project-file=$(PWD)/cabal.project

# Using getdeps.py flavor for hsthrift, we install dependencies locally, so a
# lot more flags needed
ifeq ($(BUILD_DEPS),1)
    empty :=
    space := $(empty) $(empty)
    BUILDER := hsthrift/build.sh

    DEPS_INSTALLDIR := $(patsubst %/hsthrift,%,\
            $(shell $(BUILDER) show-inst-dir hsthrift))
    DEPS := $(shell $(BUILDER) show-inst-dir hsthrift --recursive)
    LIBDIRS := $(patsubst %,--extra-lib-dirs=%/lib,$(DEPS))
    INCLUDEDIRS := $(patsubst %,--extra-include-dirs=%/include,$(DEPS))
    PKG_CONFIG_PATH := $(subst $(space),:,\
            $(shell find $(DEPS_INSTALLDIR) -name pkgconfig -type d))
    LD_LIBRARY_PATH := $(subst $(space),:,$(patsubst %,%/lib,$(DEPS)))

    THRIFT1 := $(patsubst %,%/bin/thrift1,\
        $(shell $(BUILDER) show-inst-dir fbthrift))

    CABAL=env PKG_CONFIG_PATH="$(PKG_CONFIG_PATH)" \
          LD_LIBRARY_PATH="$(LD_LIBRARY_PATH)" \
          $(CABAL_BIN) $(LIBDIRS) $(INCLUDEDIRS) $(CABAL_PROJECT)
else
    THRIFT1 := thrift1
    CABAL = $(CABAL_BIN) $(CABAL_PROJECT)
endif

THRIFT_COMPILE = $(CABAL) run exe:thrift-compiler --

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
	code_cxx \
	code_erlang \
	code_flow \
	code_hack \
	code_hs \
	code_java \
	code_rust \
	code_thrift \
	code_buck \
	codemarkup \
	code_pp \
	code_python \
	code \
	cxx1 \
	erlang \
	flow \
	glean_test \
	graphql \
	hack \
	hs \
	java \
	pp1 \
	python \
	rust \
	search_cxx \
	search_hack \
	search_pp \
	src \
	sys \
	thrift \

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
thrift-glean-hs:
	for f in $(THRIFT_GLEAN); do \
		$(THRIFT_COMPILE) --hs $$f -o $$(dirname $$f); \
	done
	# internal goes in a subdir, so do it separately
	$(THRIFT_COMPILE) --hs glean/if/internal.thrift \
		-o glean/if/internal
	# index goes in a subdir, so do it separately
	$(THRIFT_COMPILE) --hs glean/if/index.thrift \
		-o glean/if/index

.PHONY: thrift-schema-hs
thrift-schema-hs:
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
		$(THRIFT1) -I . --gen mstch_cpp2 -o $$(dirname $$f) $$f; \
	done

.PHONY: thrift-hsthrift-cpp
thrift-hsthrift-cpp::
	(cd hsthrift && make CABAL="$(CABAL)" thrift-cpp)
