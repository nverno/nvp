EMACS ?= emacs

LOAD_PATH ?=
LOAD_PATH += -L .
BATCH = ${EMACS} -Q -batch ${LOAD_PATH}

.PHONY: test
all: test
test: 
	$(BATCH) -l ert -l test/yaml-indent-test.el -f ert-run-tests-batch-and-exit
