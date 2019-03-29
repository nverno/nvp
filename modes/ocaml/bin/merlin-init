#!/usr/bin/env bash

# https://gist.github.com/unhammer/c1ac7320141f09ac38e0
# Create .merlin file for project with all ocamlfind packages and .opam
# sources

if [ -f .merlin ]; then
    printf ".merlin already exists... peace\n"
    exit 1
else 
    # Add default EXT's to list here
    cat >.merlin <<EOF
S .
B _build
EOF
    
    # Add PKG's
    ocamlfind list                     \
        | awk '{print "PKG "$1 }' >> .merlin
    
    # See https://github.com/the-lambda-church/merlin/wiki/
    # Letting-merlin-locate-go-to-stuff-in-.opam
    find ~/.opam -name '*.cmt' -print0 \
	| xargs -0 -I{} dirname '{}'   \
	| sort -u                      \
	| awk '{ print "S "$0"\nB "$0 }' >> .merlin
fi
