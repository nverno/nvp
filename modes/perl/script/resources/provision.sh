#!/usr/bin/env bash

# get resources:
set -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"

# repos
declare -A repos=(
    ["https://github.com/syohex/emacs-reply"]="reply"
)

# get / update resource repos
get_resource_repos () {
    for repo in "${!repos[@]}"; do
        if [[ ! -d "${repos["$repo"]}" ]]; then
            git clone --depth 1 "$repo" "$DIR/${repos["$repo"]}"
        else
            cd "${repos["$repo"]}"
            git pull --depth 1
        fi
    done
}

get_resource_repos
