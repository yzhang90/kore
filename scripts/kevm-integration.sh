#!/usr/bin/env bash

set -exuo pipefail

export TOP=${TOP:-$(git rev-parse --show-toplevel)}
export EVM_SEMANTICS=$TOP/.build/evm-semantics

rm -rf $EVM_SEMANTICS
git clone 'https://github.com/kframework/evm-semantics' $EVM_SEMANTICS --branch 'master'
cd $EVM_SEMANTICS
git submodule update --init --recursive

(   cd .build/k
    (   cd haskell-backend/src/main/native/haskell-backend
        git fetch $TOP
        git checkout FETCH_HEAD
    )
    git add haskell-backend/src/main/native/haskell-backend
    git commit -m '!!! haskell-backend/src/main/native/haskell-backend: integration testing haskell backend'
)

git add .build/k
git commit -m '!!! .build/k: integration testing haskell backend'

make clean
make deps          -B
make build-haskell -B
make test-vm-haskell -j8
