PKG    = nvp
EL     = $(filter-out %-autoloads.el, $(wildcard *.el))
ELC    = ${EL:.el=.elc}
EMACS  ?= emacs
SCRIPT = script
LATEX_ABBREVS = etc/unicode-latex-abbrev-table

LOAD_PATH ?= -L $(HOME)/.emacs.d/lisp
LOAD_PATH += -L .
