#!/usr/bin/env bash


DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
EMACSDIR=${EMACSDIR:-$DIR/../../..}
EMACS=${EMACS:-emacs}

IFS='' read -r -d '' INIT <<'EOS'
EOS

# ${EMACS} -nw --batch --eval
