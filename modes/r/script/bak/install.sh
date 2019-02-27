#!/usr/bin/env bash

set -e

. ~/bin/utils.sh
. ~/bin/install/install.sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
cd "$DIR"

install_R_deps() {
    if ! cat /etc/apt/sources.list | grep cran.rstudio.com; then
        local ver=_get_ubuntu_codename
        sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu $ver/" \
             >> /etc/apt/sources.list
        gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
        gpg -a --export E084DAB9 | sudo apt-key add -
    fi
    sudo apt-get -qq update

    # build deps
    sudo apt-get build-dep -y r-base

    # git2r/httr
    sudo apt-get install -y openssl
}

install_R() {
    if hash R 2>/dev/null ; then
        _log "Already have R... peace"
        exit 0
    fi
    sudo apt-get -y install r-base
}

install_R_packages() {
    if ! hash Rscript 2>/dev/null ; then
        _error_exit "Rscript not found... peace"
    fi
    Rscript "$DIR/install.R"
}

install() {
    # dotfiles
    if [ -d "$HOME/dotfiles" ]; then
        cp "$HOME/dotfiles/R/_Renviron" "$HOME/.Renviron"
        cp "$HOME/dotfiles/R/_Rprofile" "$HOME/.Rprofile"
        
        # replace environment variables in .Renviron
        local re="s;\$DEVEL;$DEVEL;"
        sed -i $re "$HOME/.Renviron"
    fi

    # install R
    install_asdf
    if ! asdf plugin-list | grep R ; then
        asdf plugin-add R https://github.com/nverno/asdf-R.git
    fi
    if ! hash R 2>/dev/null ; then
        # install latest devel version
        asdf_install_latest R "devel_[-0-9A-Za-z._]*"
    fi

    # install default packages
    install_R_packages
}

"$@"
