RM    = rm -rf
emacs ?= emacs
wget  ?= wget

.PHONY: test nvp
all: test
test: nvp
	$(emacs) -Q -batch --eval                              \
	'(progn (push "." load-path) (push "nvp" load-path))'  \
	-L . -l ert -l test/scheme-tests.el                    \
	-f ert-run-tests-batch-and-exit
	$(RM) nvp

.INTERMEDIATE: nvp
nvp:
	@if [ ! -d nvp ]; then                                 \
	  git clone --depth=1 "https://github.com/nverno/nvp"; \
	fi

README.md : el2markdown.el nvp-scheme.el
	$(emacs) -batch -l $< nvp-scheme.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~
