emacs ?= emacs

.PHONY: test
all: test
test: 
	$(emacs) -Q -batch -L . -l ert -l test/yaml-indent-tests.el \
	-f ert-run-tests-batch-and-exit
