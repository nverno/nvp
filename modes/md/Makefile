RM    = rm -rf
emacs ?= emacs
wget  ?= wget
EL    = $(wildcard *.el)

.PHONY: clean distclean compile
all: compile
compile: nvp $(EL)
	$(emacs) -Q -batch -L . -L nvp -f batch-byte-compile $(EL)

.INTERMEDIATE: nvp
nvp:
	if [ ! -d nvp ]; then                        \
	  git clone "https://github.com/nverno/nvp"; \
	fi

README.md: el2markdown.el nvp-md.el
	$(emacs) -batch -l $< nvp-md.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~ nvp

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
