curl  ?= curl
emacs ?= emacs
wget  ?= wget
RM     = rm -rf

.PHONY: nvp test
all: test
test: nvp
	$(emacs) -Q -batch --eval '(progn (push "nvp" load-path))' \
	-L . -l ert -l test/shell-tests.el                         \
	-f ert-run-tests-batch-and-exit
	$(RM) -rf nvp

nvp:
	if [ ! -d nvp ]; then                                      \
	  git clone "https://github.com/nverno/nvp";               \
	fi

README.md : el2markdown.el shell-tools.el
	$(emacs) -batch -l $< shell-tools.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~
