#!/usr/bin/env bash

set -o nounset -o pipefail -o errexit

. ~/bin/utils.sh
. ~/bin/install/install.sh

install() {
    # deps
    # install_sml_deps
    
    install_asdf
    if ! asdf plugin-list | grep sml; then
        asdf plugin-add sml https://github.com/nverno/asdf-sml.git
    fi
    
    # build latest sml
    if ! asdf current sml 2>&1 | grep "not installed"; then
        asdf_install_latest sml 
    else
        local ver=$(asdf current sml)
        _log "Current sml: $ver"
    fi
    
    # source
    # install_sml_source
}

install_sml_deps() {
    sudo apt-get install -y subversion gcc-multilib g++-multilib lib32ncurses5 lib32z1
}

# download SML source
install_sml_source () {
    local sml_svn="https://smlnj-gforge.cs.uchicago.edu/svn"
    local outdir="$DEVEL/sml"
    local repo="$sml_svn/smlnj"
    
    if [ ! -d "$DEVEL/sml" ]; then
        svn export --username anonsvn --password anonsvn $repo/admin "$outdir"
        (
            cd "$outdir"
            ./checkout-all.sh --export
        )
    else 
        _log "$outdir already exists"
    fi
}

"$@"
