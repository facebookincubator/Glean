#!/usr/bin/env sh

git clone https://github.com/facebook/wangle.git tmp >& /dev/null
cd tmp && git tag | sort -r | head -1 && cd ..
rm -rf tmp
