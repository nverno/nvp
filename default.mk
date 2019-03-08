SHELL = /bin/bash
PKG   = nvp
EL    = $(filter-out %-autoloads.el, $(wildcard *.el))
ELC   = ${EL:.el=.elc}
EMACS  ?= emacs
BIN   = bin
LATEX_ABBREVS = etc/unicode-latex-abbrev-table

# Include site variables
LOAD_PATH ?= -L $(HOME)/.emacs.d/lisp
LOAD_PATH ?= -L $(HOME)/.emacs.d/private
LOAD_PATH += -L .
