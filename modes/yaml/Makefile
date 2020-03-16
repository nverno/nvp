-include ../../config.mk
include ../../default.mk

LOAD_PATH += -L .
TESTS := $(addprefix  -l ,$(wildcard test/*.el))

.PHONY: test
all: test
test: 
	$(BATCH) -l ert ${TESTS} -f ert-run-tests-batch-and-exit
