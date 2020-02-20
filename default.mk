SHELL ?= /bin/bash
EMACS ?= emacs
AWK   ?= gawk
SED   ?= sed
CASK  ?= cask
ETAGS ?= ctags

CP    ?= install -p -m 644
MKDIR ?= install -p -d                            #755 regardless

INSTALL_INFO ?= $(shell command -v ginstall-info || printf install-info)
MAKEINFO     ?= makeinfo

PKG           = nvp
EL            = $(filter-out %-autoloads.el, $(wildcard *.el)) $(wildcard macs/*.el)
ELC           = ${EL:.el=.elc}
BIN           = bin
LATEX_ABBREVS = etc/unicode-latex-abbrev-table

# Include site variables
EMACSDIR = $(HOME)/.emacs.d
LISPDIR  = $(EMACSDIR)/lisp
SITEDIR  = $(EMACSDIR)/site-lisp
PRIVDIR  = $(EMACSDIR)/private
ELPADIR  = $(EMACSDIR)/elpa

LOAD_PATH ?= 
LOAD_PATH += -L .
LOAD_PATH += -L $(LISPDIR)
LOAD_PATH += -L $(SITEDIR)
LOAD_PATH += -L $(PRIVDIR)
LOAD_PATH += -L $(CURDIR)/test
LOAD_PATH += -L $(ELPADIR)
LOAD_PATH += -L $(SITEDIR)/nvp
LOAD_PATH += -L $(SITEDIR)/company-autoconf
LOAD_PATH += -L $(SITEDIR)/llvm-mode
LOAD_PATH += -L $(SITEDIR)/oat-mode

BATCH   = ${EMACS} -Q --batch ${LOAD_PATH} \
	-l $(SITEDIR)/nvp-build-site.el -f nvp-build-paths
COMPILE = ${BATCH} -f batch-byte-compile $(1)
ifdef USE_CASK
BATCH   = $(CASK) exec $(BATCH)
COMPILE = $(CASK) exec ${COMPILE} 
endif
