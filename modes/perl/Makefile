emacs ?= emacs
wget  ?= wget

.PHONY: nvp test
all: test
test: nvp
	$(emacs) -Q -batch --eval '(progn (push "nvp" load-path))' \
	-L . -l ert -l test/perl-tests.el                          \
	-f ert-run-tests-batch-and-exit
	$(RM) nvp

nvp:
	if [ ! -d nvp ]; then                                      \
	  git clone "https://github.com/nverno/nvp";               \
	fi

README.md : el2markdown.el perl-tools.el
	$(emacs) -batch -l $< perl-tools.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~
