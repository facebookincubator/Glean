# MARK: build: deps+setup
FROM ubuntu:24.04 AS build
WORKDIR /build

# This is what is tested at Meta, so we use it here.
ENV GHC_VER=9.2.8
# See: https://github.com/haskell/cabal/issues/10046
ENV CABAL_VER=3.10
# Major available version of Clang.
ENV CLANG_VER=15
# Must be set for GHC, otherwise cabal will fail to build files with non-ASCII
# characters (like lens).
ENV LANG=en_US.UTF-8

# Install base dependencies.
RUN apt update \
 && apt install -y \
      curl locales build-essential clang-$CLANG_VER git ninja-build make cmake libboost-all-dev \
      libgmp-dev libaio-dev libbz2-dev libdouble-conversion-dev libdwarf-dev libgoogle-glog-dev libiberty-dev libjemalloc-dev \
      libnuma-dev liblzma-dev liblz4-dev libsnappy-dev libsodium-dev libssl-dev libunwind-dev libzstd-dev libfast-float-dev \
      pkg-config rsync libgmock-dev libpcre3-dev libtinfo-dev libxxhash-dev patchelf \
 && locale-gen en_US.UTF-8 && update-locale LANG=en_US.UTF-8
 
# Install GHC and Cabal via ghcup.
RUN curl --proto '=https' --tlsv1.2 -sSf "https://downloads.haskell.org/~ghcup/$(uname -m)-linux-ghcup" -o /tmp/ghcup \
 && chmod +x /tmp/ghcup \
 && /tmp/ghcup install cabal $CABAL_VER --set \
 && /tmp/ghcup install ghc $GHC_VER --set

# Use Clang, not gcc. Note that this MUST happen *AFTER* installing GHC above.
# Apparently `ghcup` presumably modifies the settings for GHC in some way, which
# inevitably causes a link failure due to incompatible -fPIC vs non-fPIC build
# settings...
RUN apt remove -y gcc g++ && apt autoremove -y \
 && update-alternatives --install /usr/bin/cc  cc  /usr/bin/clang-$CLANG_VER 10 \
 && update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-$CLANG_VER 10 \
 && update-alternatives --install /usr/bin/gcc gcc /usr/bin/clang-$CLANG_VER 10 \
 && update-alternatives --install /usr/bin/g++ g++ /usr/bin/clang++-$CLANG_VER 10

# Run deps build (rocksdb, folly, hsthrift, etc). install_deps.sh needs some of
# the source files, so copy the minimum amount needed so that we can skip this
# step as much as possible (because these files are not changed) as frequently.)
COPY ./mk ./mk
COPY install_deps.sh Makefile cabal.project glean.cabal.in ./
RUN ./install_deps.sh --threads $(nproc)
RUN rm -rf /tmp/fbcode_builder_getdeps-Z__wZGleanZGleanZhsthriftZbuildZfbcode_builder-root/

# MARK: build: main build
COPY ./thrift/ ./thrift
COPY ./glean ./glean
RUN touch settings.mk \
 && echo "EXTRA_GHC_OPTS=-j$(nproc) +RTS -A128m -n2m -RTS" >> settings.mk \
 && echo "CABAL_CONFIG_FLAGS=-fclang -f-hack-tests -f-rust-tests -f-python-tests" >> settings.mk
RUN true && \
    ( export LD_LIBRARY_PATH=$HOME/.hsthrift/lib:$LD_LIBRARY_PATH \
    ; export PKG_CONFIG_PATH=$HOME/.hsthrift/lib/pkgconfig:$PKG_CONFIG_PATH \
    ; export PATH=$HOME/.ghcup/bin:$HOME/.hsthrift/bin:$PATH \
    ; cabal update && make -j$(nproc) && make bindist \
    )
# Now patch the binaries to include /usr/local/lib in their rpath, as that's
# where the final libraries will go anyway.
RUN patchelf --add-rpath /usr/local/lib ./bindir/bin/*

# MARK: common: base image
FROM ubuntu:24.04 AS common
# This image only exists so that the client and server images can share it.
COPY --from=build /build/bindir/lib/* /usr/local/lib
RUN mkdir -p /glean/db /glean/schema \
 && apt update \
 && apt install -y \
      libatomic1 \
      libsnappy1v5 \
      libpcre3 \
      libunwind8 \
      libgoogle-glog0v6t64 \
      libicu74 \
      libdouble-conversion3 \  
      libevent-2.1-7t64 \
      libdwarf1 \
      libsodium23 \
      libaio1t64 \
      libboost-context1.83.0 \
      libboost-regex1.83.0 \
      libboost-program-options1.83.0 \
      libboost-system1.83.0 \
      libboost-thread1.83.0 \
      libboost-atomic1.83.0 \
      libboost-filesystem1.83.0

# MARK: server+client
FROM common AS client
COPY --from=build /build/bindir/bin/glean /usr/local/bin
CMD ["/usr/local/bin/glean", "--schema", "dir:/glean/schema"]

FROM client AS server
COPY --from=build /build/bindir/bin/glean-server /usr/local/bin
CMD ["/usr/local/bin/glean-server", "--db-root", "/glean/db", "--schema", "dir:/glean/schema", "--port", "12345"]

FROM common AS tools
COPY --from=build /build/bindir/bin/glean /usr/local/bin
COPY --from=build /build/bindir/bin/gen-schema /usr/local/bin/glean-gen-schema

# MARK: demo setup
FROM common AS demo-build
RUN apt update && apt install -y unzip git
ADD https://github.com/facebook/flow/releases/download/v0.245.2/flow-linux64-v0.245.2.zip /tmp/flow-x86_64.zip
ADD https://github.com/facebook/flow/releases/download/v0.245.2/flow-linux-arm64-v0.245.2.zip /tmp/flow-aarch64.zip
RUN true && (cd /tmp; unzip flow-$(uname -m).zip)

RUN true \
 && git clone --depth 1 --branch v19.0.0 https://github.com/facebook/react.git /react-code \
 && rm -rf /react-code/.git
RUN cat /react-code/scripts/flow/config/flowconfig \
      | grep -v REACT_RENDERER_FLOW_ \
      | grep -v FLOW_VERSION \
      | sed '/^\[options\]/a exact_by_default=false' > /react-code/.flowconfig

FROM server AS demo
COPY --from=build /build/bindir/bin/glean-hyperlink /usr/local/bin
COPY --from=build /build/glean/schema/source /glean/schema/
COPY --from=demo-build /tmp/flow/flow /usr/local/bin
COPY --from=demo-build /react-code /glean/code/react

WORKDIR /glean
RUN mkdir -p /tmp/flow-index-out \
 && flow glean code/react --output-dir /tmp/flow-index-out --write-root "" \
 && glean --db-root db --schema dir:schema create --repo react/0 \
 && glean --db-root db --schema dir:schema write --repo react/0 /tmp/flow-index-out/* \
 && glean --db-root db --schema dir:schema derive --repo react/0 flow.FileXRef flow.FileDeclaration \
 && glean --db-root db --schema dir:schema finish --repo react/0 \
 && rm -rf /tmp/flow-index-out

# write out a simple inline script to start the demo app
RUN touch /usr/local/bin/glean-demo \
 && chmod a+x /usr/local/bin/glean-demo \
 && echo "#!/usr/bin/env bash" >> /usr/local/bin/glean-demo \
 && echo "set -ex" >> /usr/local/bin/glean-demo \
 && echo "/usr/local/bin/glean-server --db-root /glean/db --schema dir:/glean/schema --port 12345 &" >> /usr/local/bin/glean-demo \
 && echo "sleep 3" >> /usr/local/bin/glean-demo \
 && echo "exec /usr/local/bin/glean-hyperlink --service localhost:12345 --repo react --root /glean/code/react --http 8888" >> /usr/local/bin/glean-demo \
 # Add a dummy favicon.ico so that the web server doesn't complain. \
 && touch /glean/code/react/favicon.ico

EXPOSE 8888
EXPOSE 12345
ENTRYPOINT ["/usr/local/bin/glean-demo"]
