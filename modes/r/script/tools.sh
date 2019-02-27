#!/usr/bin/env bash

. ~/bin/utils.sh

##
# @info		create R package
# @usage	create: package_name
# @param	package_name: name of R package
##
create () {
    Rscript -e "devtools::create(\"$1\")"
}

"$@"
