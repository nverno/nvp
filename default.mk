# SHELL ?= /bin/bash
TOP          := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
EMACS        ?= emacs
AWK          ?= gawk
SED          ?= sed
CASK         ?= cask
ETAGS        ?= ctags
CP           ?= install -p -m 644
MKDIR        ?= install -p -d #755 regardless
MAKEINFO     ?= makeinfo
INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)

PKG  = nvp
EL   = $(filter-out %-autoloads.el, $(wildcard *.el)) $(wildcard macs/*.el) \
		$(wildcard subrs/*.el)
ELC  = ${EL:.el=.elc}
BIN  = bin

LATEX_ABBREVS  = $(TOP)/abbrevs/unicode-latex-abbrev-table

# Include site variables
EMACSDIR  = $(HOME)/.emacs.d
LISPDIR   = $(EMACSDIR)/lisp
SITEDIR   = $(EMACSDIR)/site-lisp
PRIVDIR   = $(EMACSDIR)/private
ELPADIR   = $(EMACSDIR)/elpa
BINDIR    = $(TOP)/bin

LOAD_PATH ?= 
LOAD_PATH += -L .
LOAD_PATH += -L $(LISPDIR)
LOAD_PATH += -L $(SITEDIR)
LOAD_PATH += -L $(PRIVDIR)
LOAD_PATH += -L $(CURDIR)/test
LOAD_PATH += -L $(ELPADIR)
LOAD_PATH += -L $(SITEDIR)/nvp

BATCH    = ${EMACS} -Q --batch ${LOAD_PATH}      \
		--eval "(setq debug-on-error t)" \
		-l $(SITEDIR)/nvp-build-site.el -f nvp-build-paths
COMPILE  = ${BATCH} -f batch-byte-compile $(1)

ifdef USE_CASK
  BATCH    = $(CASK) exec $(BATCH)
  COMPILE  = $(CASK) exec ${COMPILE} 
endif
