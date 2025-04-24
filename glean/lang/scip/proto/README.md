Regenerating those bindings is pretty manual at this point. To do so:

Install dependencies:

```
sudo dnf install ghc cabal-install protobuf-compiler
cabal update
```

Look up which stackage LTS version is the latest:

```
ls ~/fbsource/third-party/tp2/stackage-lts
```

20.3 at the time of writing.

Find the version of proto-lens-protoc that's in that stackage LTS version:
https://www.stackage.org/lts-20.3. That's proto-lens-protoc-0.7.1.1 at the time
of writing.

```
version=0.7.1.1 # adapt this (read above)
cabal install proto-lens-protoc-$version

# clone the scip repo
mkdir -p ~/local/sources
cd ~/local/sources
git clone https://github.com/sourcegraph/scip
cd scip

# adapt the path below
protoc_haskell=$HOME/.cabal/store/ghc-8.10.7/proto-lens-protoc-0.7.1.1-d7d69820940bf6ceaed521f79ffaa7a1c41060a8e5ca6d11b1b1ac6fec1bb414/bin/proto-lens-protoc

output_dir=$HOME/local/scip-hs
mkdir -p $output_dir

# generate the haskell bindings
protoc --plugin=protoc-gen-haskell=$protoc_haskell --haskell_out=$output_dir scip.proto

# import into fbcode
cp $output_dir/Proto/* ~/fbsource/fbcode/glean/lang/scip/proto/Proto/
```

Test with

```
buck build fbcode//glean/lang/scip:scip
```
