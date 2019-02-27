#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
pushd "$DIR">/dev/null

build_ml_doc () {
    pushd "$DIR/ml-doc/">/dev/null
    ./configure
    popd>/dev/null
}

popd>/dev/null
