FROM ghcr.io/facebookincubator/hsthrift/ci-base:latest as tools
# remove any old stuff
RUN rm -rf /usr/local/lib
RUN rm -rf /usr/local/include
RUN apt-get install -y ghc-8.10.2 cmake ninja-build libxxhash-dev wget unzip rsync libgmock-dev
RUN cabal update
RUN mkdir /glean-code
WORKDIR /glean-code
ADD https://api.github.com/repos/facebookincubator/hsthrift/compare/main...HEAD /dev/null
ADD ./glean /glean-code/glean
ADD ./thrift /glean-code/thrift
ADD ./cabal.project /glean-code/
ADD ./Makefile  /glean-code/
ADD ./mk /glean-code/mk
ADD ./glean.cabal.in /glean-code/
ADD ./LICENSE /glean-code/
ADD ./Setup.hs /glean-code/
ADD ./install_deps.sh /glean-code/
RUN ./install_deps.sh
# Nuke build artifacts to save space
RUN rm -rf /tmp/fbcode_builder_getdeps-Z__wZGleanZGleanZhsthriftZbuildZfbcode_builder-root/

ENV LD_LIBRARY_PATH=/root/.hsthrift/lib
ENV PKG_CONFIG_PATH=/root/.hsthrift/lib/pkgconfig
ENV PATH=$PATH:/root/.hsthrift/bin

RUN make
RUN cp $(cabal exec --project-file=cabal.project -- which glean) ~/.cabal/bin/
RUN cp $(cabal exec --project-file=cabal.project -- which glean-server) ~/.cabal/bin/
RUN cp $(cabal exec --project-file=cabal.project -- which glean-hyperlink) ~/.cabal/bin/
RUN glean --help
RUN wget https://github.com/facebook/flow/releases/download/v0.219.0/flow-linux64-v0.219.0.zip
RUN unzip flow-linux64-v0.219.0.zip
RUN mkdir -p /root/.hsthrift/bin && mv flow/flow /root/.hsthrift/bin/ && rm -rf flow-linux64-v0.219.0.zip flow/
WORKDIR /
RUN git clone https://github.com/facebook/react.git --depth 1 react-code
RUN cat /react-code/scripts/flow/config/flowconfig \
      | grep -v REACT_RENDERER_FLOW_ \
      | grep -v CI_MAX_WORKERS \
      | grep -v FLOW_VERSION \
      | sed '/^\[options\]/a exact_by_default=false' > /react-code/.flowconfig

FROM ubuntu:20.04 AS demo

LABEL org.opencontainers.image.source="https://github.com/facebookincubator/Glean"

ENV PATH=/glean-demo/bin:$PATH
ENV LD_LIBRARY_PATH=/usr/local/lib

RUN apt-get update && apt-get install -y \
    libicu66 \
    libboost-context1.71.0 \
    libboost-filesystem1.71.0 \
    libboost-program-options1.71.0 \
    libboost-regex1.71.0 \
    libunwind8 \
    libgoogle-glog0v5 \
    libssl1.1 \
    libsodium23 \
    libdouble-conversion3 \
    libmysqlclient21 \
    libevent-2.1-7 \
    libsnappy-dev \
    libxxhash0 \
    libatomic1 \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /glean-demo

RUN mkdir /glean-demo/bin

COPY --from=tools /root/.hsthrift/lib /usr/local/lib
COPY --from=tools /root/.hsthrift/bin/flow /usr/local/bin
COPY --from=tools /root/.cabal/bin/glean /glean-demo/bin
COPY --from=tools /root/.cabal/bin/glean-server /glean-demo/bin
COPY --from=tools /root/.cabal/bin/glean-hyperlink /glean-demo/bin
COPY --from=tools /glean-code/glean/schema /glean-demo/schema
COPY --from=tools /react-code /glean-demo/code
ADD docker_entrypoint.sh docker_entrypoint.sh

RUN mkdir -p db /tmp/flow-index-out

RUN flow glean code --output-dir /tmp/flow-index-out --write-root "" && \
    glean --db-root db --schema dir:schema/source create --repo react/0 && \
    glean --db-root db --schema dir:schema/source write --repo react/0 /tmp/flow-index-out/* && \
    glean --db-root db --schema dir:schema/source derive --repo react/0 flow.FileXRef flow.FileDeclaration && \
    glean --db-root db --schema dir:schema/source finish --repo react/0 && \
    rm -Rf /tmp/flow-index-out

ENV REPO_NAME=react

EXPOSE 8888

ENTRYPOINT ["./docker_entrypoint.sh"]

# docker run -ti -p8888:8888 ghcr.io/facebookincubator/glean/demo
