FROM ghcr.io/facebookincubator/hsthrift/ci-base:latest
RUN apt-get install -y ghc-8.10.2 librocksdb-dev wget unzip
RUN cabal update
RUN mkdir /glean-code
ADD ./install_deps.sh /glean-code/
WORKDIR /glean-code
RUN ./install_deps.sh
ADD ./glean /glean-code/glean
ADD ./cabal.project /glean-code/
ADD ./Makefile  /glean-code/
ADD ./glean.cabal /glean-code/
ADD ./LICENSE /glean-code/
ADD ./Setup.hs /glean-code/
RUN make thrift && make gen-bytecode && make gen-schema && make thrift-schema-hs
RUN cabal new-build --project-file=cabal.project exe:glean exe:shell exe:hyperlink
RUN cp $(cabal exec --project-file=cabal.project -- which glean) ~/.cabal/bin/
RUN cp $(cabal exec --project-file=cabal.project -- which shell) ~/.cabal/bin/
RUN cp $(cabal exec --project-file=cabal.project -- which hyperlink) ~/.cabal/bin/
RUN glean --help
RUN apt-get install -y wget unzip
RUN wget https://github.com/facebook/flow/releases/download/v0.148.0/flow-linux64-v0.148.0.zip
RUN unzip flow-linux64-v0.148.0.zip
RUN mv flow/flow /usr/local/bin/ && rm -rf flow-linux64-v0.148.0.zip flow/
WORKDIR /
RUN git clone https://github.com/facebook/react.git --depth 1 react-code

RUN mkdir -p /gleandb /tmp/react
RUN cat /react-code/scripts/flow/config/flowconfig \
      | grep -v REACT_RENDERER_FLOW_OPTIONS \
      | grep -v suppress_comment > /react-code/.flowconfig
RUN flow glean /react-code --output-dir /tmp/react --write-root ""
RUN glean --db-root /gleandb --db-schema dir:/glean-code/glean/schema/source create --repo react/0
RUN glean --db-root /gleandb --db-schema dir:/glean-code/glean/schema/source write --repo react/0 /tmp/react/*
RUN glean --db-root /gleandb --db-schema dir:/glean-code/glean/schema/source derive --repo react/0 flow.FileXRef flow.FileDeclaration
RUN glean --db-root /gleandb --db-schema dir:/glean-code/glean/schema/source finish --repo react/0

# can then run:
#   hyperlink --db-root /gleandb --db-schema dir:/glean-code/glean/schema/source --repo react --root /react-code --http 8888
# from within the image