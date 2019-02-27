#!/usr/bin/env bash

# Downloads SML resources:
# - sml source code from svn repo
# - the sml basis library manual
# - code for a compiler written with SML for a mini-language
# - emacs-lisp repos with SMl utilities
# - ml-doc tool used to generate SML documentation
# - nsgmls, a pre-req for ml-doc

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
pushd "$DIR">/dev/null

# svn repo
sml_svn="https://smlnj-gforge.cs.uchicago.edu/svn"
smlnj="$sml_svn/smlnj"

# default directory for source files
sml_src_dir="sml-source"

# download SML source
get_sml_source () {
    command -v svn >/dev/null 2>&1 || { 
        echo >&2 "Don't have svn installed. Aborting..."; 
        exit 1;
    }
    
    local outdir="${1-$sml_src_dir}"

    svn export --username anonsvn --password anonsvn \
        $smlnj/admin "$outdir"
    pushd "$outdir">/dev/null
    ./checkout-all.sh --export
    popd>/dev/null

}

# ML-compiler resources for tiger language compiler
# Homepage: http://www.cs.princeton.edu/~appel/modern/ml/
sml_tiger_bundle="http://www.cs.princeton.edu/~appel/modern/ml/tiger.tar"

get_sml_tiger_compiler () {
    wget "$sml_tiger_bundle"
    7za x tiger.tar
    rm tigar.tar
}

# basis library manual
# manpages: http://sml-family.org/Basis/manpages.html
sml_basis_manual="http://aleteya.cs.buap.mx/~jlavalle/papers/books_on_line/The-standard-ml-basis-library.pdf"

get_sml_basis_manual () {
    wget "$sml_basis_manual"
}

# ml-doc tool
# http://people.cs.uchicago.edu/~jhr/tools/ml-doc.html
ml_doc_bundle="http://people.cs.uchicago.edu/~jhr/tools/downloads/ml-doc.tgz"

get_sml_ml_doc () {
    wget "$ml_doc_bundle"
    7za x ml-doc.tgz -so | 7za x -si -ttar
    rm ml-doc.tgz
}


# ml-doc requires nsgmls or onsgmls
# http://www.jclark.com/sp/howtoget.htm
nsgmls_win_bin="ftp://ftp.jclark.com/pub/sp/win32/sp1_3_4.zip"

# get the windows binaries
get_nsgmls_bin () {
    if [[ ! ("$OSTYPE" == "msys" || "$OSTYPE" == "cygwin") ]]; then
        cat <<EOF

Error: "provision.sh" 66.0.

Function 'get_nsgmls_bin' Needs to be adjusted to get the
non-windows version of nsgmls for ml-doc.

See http://www.jclark.com/sp/howtoget.htm
EOF
        exit 1
    else
        wget "$nsgmls_win_bin"
        7za x -tzip sp1_3_4.zip -o"nsgmls"
        rm sp1_3_4.zip
    fi
}

# repos
declare -A repos=( ["https://github.com/nverno/company-sml"]="company-sml"
                   ["https://bitbucket.org/nverno/eldoc-sml"]="eldoc-sml"
                   ["https://github.com/emacsmirror/sml-mode"]="sml-mode" 
                   ["https://github.com/nverno/sml-dash-docset"]="docset"
                 )

# get / update resource repos
get_sml_repos () {
    for repo in "${!repos[@]}"; do
        if [[ ! -d "${repos["$repo"]}" ]]; then
            git clone --depth 1 "$repo" "${repos["$repo"]}"
        else
            pushd "${repos["$repo"]}">/dev/null
            git pull --depth 1
            popd>/dev/null
        fi
    done
}

# AC word list
ac_words="https://raw.githubusercontent.com/PelleJuul/AC-SML/master/sml-mode"
get_sml_words () {
    curl -Lo words.dat $ac_words
}

# ------------------------------------------------------------
# get stuff

[[ ! -d "$sml_src_dir" ]] && get_sml_source
[[ ! -d "tiger" ]] && get_sml_tiger_compiler
[[ ! -f "The-standard-ml-basis-library.pdf" ]] && get_sml_basis_manual
[[ ! -d "ml-doc" ]] && get_sml_ml_doc
[[ ! -d "nsgmls" ]] && get_nsgmls_bin
[[ ! -f "words.dat" ]] && get_sml_words

get_sml_repos

popd>/dev/null

# Local Variables:
# sh-shell: bash
# End:
