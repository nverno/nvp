# -*- mode: sh -*-
alias lt='ls -altr'
alias ebc='emacs -Q -batch -L . -f batch-byte-compile'

alias gitcln='git clone https://github.com/nverno/'

if [[ $OS == "Windows_NT" ]]; then
    alias cdd='cd d:/'
fi
