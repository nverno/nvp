SHELL ?= /bin/bash
EMACS ?= emacs
AWK   ?= gawk
SED   ?= sed
CASK  ?= cask
ETAGS ?= etags

CP ?= install -p -m 644
MKDIR ?= install -p -d                            #755 regardless

INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO ?= makeinfo

PKG     =  nvp
EL      =  $(filter-out %-autoloads.el, $(wildcard *.el))
ELC     =  ${EL:.el=.elc}
BIN     =  bin
LATEX_ABBREVS = etc/unicode-latex-abbrev-table

# Include site variables
LOAD_PATH ?= 
LOAD_PATH += -L .
LOAD_PATH += -L $(HOME)/.emacs.d/lisp
LOAD_PATH += -L $(HOME)/.emacs.d/site-lisp
LOAD_PATH += -L $(HOME)/.emacs.d/private
LOAD_PATH += -L $(CURDIR)/test
LOAD_PATH += -L $(HOME)/.emacs.d/elpa
LOAD_PATH += -L $(HOME)/.emacs.d/site-lisp/nvp

BATCH   = ${EMACS} -Q --batch ${LOAD_PATH} \
	-l $(CURDIR)/../nvp-build-site.el -f nvp-build-paths
COMPILE = ${BATCH} -f batch-byte-compile $(1)
ifdef USE_CASK
BATCH   = $(CASK) exec $(BATCH)
COMPILE = $(CASK) exec ${COMPILE} 
endif
