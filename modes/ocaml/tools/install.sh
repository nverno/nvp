#!/usr/bin/env bash

set -e

source ~/bin/versions.sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)" 
cd "$DIR"

# TODO: 
# install aspcud

opam_installer="https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh"

install_opam() {
    if hash opam 2>/dev/null ; then
        printf "Already have opam... peace\n"
        return 0
    fi
    wget $opam_installer -O - | sh -s /usr/local/bin

    # dotfiles
    if [ -d "$HOME/dotfiles" ]; then
        cp "$HOME/dotfiles/ocaml/_ocamlinit" "$HOME/.ocamlinit"
        cp "$HOME/dotfiles/ocaml/_ocamldebug" "$HOME/.ocamldebug"
        cp "$HOME/dotfiles/ocaml/_ocp-indent" "$HOME/.ocp-indent"
    fi
}

install_opam_init() {
    if ! hash opam 2>/dev/null ; then
        printf "Failed to find opam\n"
        exit 1
    fi
    # versions at https://ocaml.org/releases/
    opam init --no-setup --build-doc --color=never --yes --comp=4.04.0
    opam init -n
}

# ocaml packages: ocp-indent, utop, ocamlmerlin, merlin
# plus core stuff: core async core_extended
install_ocaml_pkgs() {
    if ! hash opam 2>/dev/null ; then
        printf "opam not found... peace\n"
        exit 1
    fi
    opam install -y --color=never ocp-indent merlin  \
         core async core_extended                    \
         utop
}
 
install_ocaml_src() {
    [[ ! -d "$DEVEL" ]] && (
        printf "DEVEL not set... peace\n"
        exit 1
    )
    
    cd "$DEVEL"
    if [ ! -d ocaml ]; then
        git clone --depth=1 "https://github.com/ocaml/ocaml"
    fi
}

install_ocaml_info() {
    local url ref
    # http://caml.inria.fr/pub/distrib/ocaml-4.03/ocaml-4.03-refman.info.tar.gz
    url="http://caml.inria.fr/download.en.html"
    ref=$(curl -Ls $url | grep -Poi "pub.*refman\.info\.tar\.gz")

    if [ -n "$ref" ]; then
        mkdir _build
        pushd _build >/dev/null
        curl "http://caml.inria.fr/$ref" | tar xz
        [[ ! -d infoman ]] && (
            printf "Failed to download/unpack $ref\n"
            exit 1
        )
        sudo mv infoman/*.info.*.gz /usr/share/info
        sudo install-info --info-dir=/usr/share/info \
             --info-file=/usr/share/info/ocaml.info.gz
        popd >/dev/null
        rm -rf _build
    fi
}

"$@"
