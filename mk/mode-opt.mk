# opt mode - optimised builds + benchmarks

CABAL_CONFIG_FLAGS = --builddir=$(PWD)/$(MODE_DIR)/dist-newbuild --flags=opt --flags=benchmarks
CXX_MODE=make
export CXXFLAGS += -O3 -DNDEBUG
