#!/usr/bin/env bash

# get resources:
set -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
pushd "$DIR">/dev/null

# files
files=(
    "https://lagunita.stanford.edu/c4x/Engineering/Compilers/asset/cool-tour.pdf"
    "https://lagunita.stanford.edu/c4x/Engineering/Compilers/asset/cool_manual.pdf"
    "https://s3-us-west-1.amazonaws.com/prod-edx/Compilers/ProgrammingAssignments/PA1.pdf"
    "https://lagunita.stanford.edu/c4x/Engineering/Compilers/asset/pa1-grading.pl"
    "https://lagunita.stanford.edu/c4x/Engineering/Compilers/asset/c__.ps"    
)

# repos
declare -A repos=( 
    ["https://github.com/nverno/flex-mode"]="flex-mode"
    ["https://github.com/nverno/bison-mode"]="bison-mode"
    ["https://bitbucket.org/nverno/compiler"]="compiler"
)

# get files
get_resource_files () {
    for f in "${files[@]}"; do
        if [[ ! -f "$(basename "$f")" ]]; then
            wget "$f"
        fi
    done
}

# get / update resource repos
get_resource_repos () {
    for repo in "${!repos[@]}"; do
        if [[ ! -d "${repos["$repo"]}" ]]; then
            git clone --depth 1 "$repo" "${repos["$repo"]}"
        else
            cd "${repos["$repo"]}"
            git pull --depth 1
        fi
    done
}

get_resource_repos
get_resource_files

# Local Variables:
# sh-shell: bash
# End:
