#!/usr/bin/env bash

set -o nounset -o pipefail -o errexit

DIR="$(cd "$( dirname "${BASH_SOURCE[0]}")" && pwd)"
SCRIPT="$DIR/add-locals.awk"

if [[ -z "$*" ]]; then
    echo "Missing directories to search in"
    exit 1
fi

find "$@" -type f -iname '*.el' \
     -exec awk -i inplace -v inplace::suffix=".orig" -f "$SCRIPT" '{}' \;
