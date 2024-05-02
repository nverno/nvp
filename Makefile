SHELL  = /bin/bash
-include config.mk
include default.mk

SRCDIRS := $(shell find modes/ pkgs/ -mindepth 1 -maxdepth 1 -type d)
PKGS    := $(sort $(notdir ${SRCDIRS}))
PKG_EL  := $(shell find modes/ pkgs/ -type f -name \*.el                    \
		\! \( -path \*/snippets/* -o -name .\* -o -path \*/unused/* \
		-o -path \*/test/\* -o -path \*/etc/\* -o -path \*/w32/\*   \
		-o -path \*/ext/\* -o -path \*/build/\* -o -path \*/scratch/\* \))
PKG_ELC := ${PKG_EL:.el=.elc}

FILTER_MODE  = $(filter $(addprefix modes/,$(1))/%,$(2))
CLEAN_MODE   = $(info cleaning $(1))    \
	@$(RM) $(call FILTER_MODE,$(1),${PKG_ELC})
BUILD_PKG    = $(info building $(mode)) \
	@$(call COMPILE,$(call FILTER_MODE,$(1),${PKG_EL}))
TESTS       := $(wildcard test/*-tests.el)

.PHONY: test rebuild all
all: pre-commit
	$(COMPILE) $(EL) $(PKG_EL)

%.elc: %.el
	$(COMPILE) $^

pre-commit: $(TOP)/.git/hooks/pre-commit
$(TOP)/.git/hooks/pre-commit: $(BINDIR)/pre-commit
	@cp $^ $@

rebuild: clean-all ## Recompile macs/subrs/and all base configs
	$(MAKE)

.PHONY: clean-pkgs build-pkgs clean-all
clean-all:
	@find $(TOP) -type f -name '*.elc' -exec rm {} \;

clean-pkgs: ## Clean all pkgs
	$(foreach mode,${PKGS},$(call CLEAN_MODE,$(mode)))

build-pkgs: ## Build all pkgs -- FIXME: does weird loopy shit
	$(foreach mode,${PKGS},$(BUILD_PKG))

.PHONY: ${PKGS}
clean-%: % ## Clean mode
	$(call CLEAN_MODE,$^)

build-%: % ## Build mode
	$(call CLEAN_MODE,$^)
	$(call BUILD_PKG,$^)

dep-%: % ## print depends for package
	@grep "$^" .depend

test: ## Run tests
	$(BATCH) -l ert $(addprefix  -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

check-compiled: ## Check compiled files for subrs/macros
	@$(CURDIR)/bin/check compiled && exit 1 || echo "check-compiled: all good"

.PHONY: unicode
unicode: ## Generate latex/unicode abbrevs
	@julia ${BIN}/latex_abbrevs.jl abbrev nil ${LATEX_ABBREVS}

# .PHONY: .depend
.depend: $(EL) $(PKG_EL) ## create depends for *.el files
	$(info Computing depends)
	@rm -f .depend
	@${BIN}/depends.awk $^ >> .depend

-include .depend

define LOADDEFS_TMPL
;;; ${PKG}-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$$) (car load-path))))


;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ${PKG}-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'

${PKG}-autoloads.el: ${EL} ## Generate package autoloads
	$(info Generating $@)
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@${BATCH} --eval "(progn                                   \
	(setq make-backup-files nil)                               \
	(setq vc-handled-backends nil)                             \
	(setq default-directory (file-truename default-directory)) \
	(setq generated-autoload-file (expand-file-name \"$@\"))   \
	(setq find-file-visit-truename t)                          \
	(update-directory-autoloads default-directory))"

TAGS: ${EL} ${PKG_EL}
	$(RM) $@
	@touch $@
	ls $^ | xargs $(ETAGS) -a -o $@

.PHONY: clean distclean
clean: ## clean temp files
	$(RM) *~ \#.*\#

distclean: clean-pkgs clean ## clean all generated files inc. compiled/auto
	$(RM) *loaddefs?.el *autoloads.el TAGS GPATH GTAGS \
		*.elc macs/*.elc subrs/*.elc

.PHONY: help
help: ## Display this help message
	@for mfile in $(MAKEFILE_LIST); do                  \
	  grep -E '^[a-zA-Z_%-]+:.*?## .*$$' $$mfile |      \
	  sort | ${AWK}                                     \
	  'BEGIN {FS = ":.*?## "};                          \
	   {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'; \
	done
