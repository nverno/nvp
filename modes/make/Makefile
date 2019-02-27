curl  ?= curl
emacs ?= emacs
wget  ?= wget
RM    = rm -rf
EL    = $(wildcard *.el)

.PHONY: nvp
all: 
test: nvp
	$(emacs) -Q -batch -L . -L nvp -f batch-byte-compile $(EL)
	$(RM) -rf nvp

.INTERMEDIATE: nvp
nvp:
	if [ ! -d nvp ]; then                                  \
	  git clone --depth=1 "https://github.com/nverno/nvp"; \
	fi

README.md : el2markdown.el nvp-make.el
	$(emacs) -batch -l $< nvp-make.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~ nvp
