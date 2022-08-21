# Building C++ libraries via Cabal
#
# Just add cxx-sources and cxx-options for each library

define CXX_DEFS_M4
m4_divert(-1)
m4_define(`CXX_EXTRA_SOURCE_FILES')
m4_define(`CXX_EXTRA_LIB_DIRS')
$(foreach lib, $(CXX_LIBRARIES),
m4_define(`CXX_LIB_$(lib)', $(subst $() $(),
        ,cxx-sources: $(patsubst %,%,$(CXX_SOURCES_$(lib))))$(if $(CXX_FLAGS_$(lib)),
    cxx-options: $(CXX_FLAGS_$(lib)))))
m4_divert(0)m4_dnl
endef
