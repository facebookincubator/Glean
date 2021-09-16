FROM ghcr.io/facebookincubator/hsthrift/ci-base:latest as tools
RUN apt-get install -y ghc-8.10.2 librocksdb-dev libxxhash-dev wget unzip
RUN cabal update
RUN mkdir /glean-code
WORKDIR /glean-code
ADD https://api.github.com/repos/facebookincubator/hsthrift/compare/main...HEAD /dev/null
ADD ./install_deps.sh /glean-code/
RUN ./install_deps.sh
ADD ./glean /glean-code/glean
ADD ./cabal.project /glean-code/
ADD ./Makefile  /glean-code/
ADD ./glean.cabal /glean-code/
ADD ./LICENSE /glean-code/
ADD ./Setup.hs /glean-code/
RUN make
RUN cp $(cabal exec --project-file=cabal.project -- which glean) ~/.cabal/bin/
RUN cp $(cabal exec --project-file=cabal.project -- which glean-server) ~/.cabal/bin/
RUN cp $(cabal exec --project-file=cabal.project -- which glean-hyperlink) ~/.cabal/bin/
RUN glean --help
RUN apt-get install -y wget unzip
RUN wget https://github.com/facebook/flow/releases/download/v0.148.0/flow-linux64-v0.148.0.zip
RUN unzip flow-linux64-v0.148.0.zip
RUN mv flow/flow /usr/local/bin/ && rm -rf flow-linux64-v0.148.0.zip flow/
WORKDIR /
RUN git clone https://github.com/facebook/react.git --depth 1 react-code
RUN cat /react-code/scripts/flow/config/flowconfig \
      | grep -v REACT_RENDERER_FLOW_OPTIONS \
      | grep -v suppress_comment > /react-code/.flowconfig

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
    librocksdb5.17 \
    libunwind8 \
    libgoogle-glog0v5 \
    libssl1.1 \
    libsodium23 \
    libdouble-conversion3 \
    libmysqlclient21 \
    libevent-2.1-7 \
    libxxhash0 \
    libatomic1 \
 && apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /glean-demo

RUN mkdir /glean-demo/bin

COPY --from=tools /usr/local/lib /usr/local/lib
COPY --from=tools /usr/local/bin/flow /usr/local/bin
COPY --from=tools /root/.cabal/bin/glean /glean-demo/bin
COPY --from=tools /root/.cabal/bin/glean-server /glean-demo/bin
COPY --from=tools /root/.cabal/bin/glean-hyperlink /glean-demo/bin
COPY --from=tools /glean-code/glean/schema /glean-demo/schema
COPY --from=tools /react-code /glean-demo/code
ADD docker_entrypoint.sh docker_entrypoint.sh

RUN mkdir -p db /tmp/flow-index-out

RUN flow glean code --output-dir /tmp/flow-index-out --write-root "" && \
    glean --db-root db --db-schema dir:schema/source create --repo react/0 && \
    glean --db-root db --db-schema dir:schema/source write --repo react/0 /tmp/flow-index-out/* && \
    glean --db-root db --db-schema dir:schema/source derive --repo react/0 flow.FileXRef flow.FileDeclaration && \
    glean --db-root db --db-schema dir:schema/source finish --repo react/0 && \
    rm -Rf /tmp/flow-index-out

ENV REPO_NAME=react

EXPOSE 8888

ENTRYPOINT ["./docker_entrypoint.sh"]

# docker run -ti -p8888:8888 ghcr.io/facebookincubator/glean/demo
