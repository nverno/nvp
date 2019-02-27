SHELL      = /bin/bash
curl       ?= curl
emacs      ?= emacs
wget       ?= wget
RM         = rm -rf
SCRIPT_DIR = script
SETUP      = setup

.PHONY: ${SETUP} nvp test
all: ${SETUP} test
${SETUP}:
	if [ -d ~/bin/install ]; then                                       \
		chmod +x ./${SCRIPT_DIR}/$@;                                \
		./${SCRIPT_DIR}/$@;                                         \
	fi

test: nvp ess
	$(emacs) -Q -batch                                                  \
	--eval "(require 'subr-x)"                                          \
	--eval '(progn (push "nvp" load-path) (push "ess/lisp" load-path))' \
	-L . -l ert -l test/r-tests.el                                      \
	-f ert-run-tests-batch-and-exit
	${RM} nvp ess

nvp:
	if [ ! -d nvp ]; then                                               \
	  git clone --depth=1 "https://github.com/nverno/nvp";              \
	fi

ess:
	if [ ! -d ess ]; then                                               \
	  git clone --depth=1 "https://github.com/emacs-ess/ess";           \
	fi
# cd ess/lisp && make

README.md : el2markdown.el r-tools.el
	$(emacs) -batch -l $< r-tools.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~
