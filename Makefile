-include config.mk
include default.mk

MODEDIRS := $(shell find modes/ -mindepth 1 -maxdepth 1 -type d)
MODES    := $(sort $(notdir ${MODEDIRS}))
MODE_EL  := $(shell find modes/ -type f -name \*.el                         \
		\! \( -path \*/snippets/* -o -name .\* -o -path \*/unused/* \
		-o -path \*/test/\* -o -path \*/etc/\* -o -path \*/w32/\*   \
		-o -path \*/ext/\* -o -path \*/build/\* -o -path \*/scratch/\* \))
MODE_ELC := ${MODE_EL:.el=.elc}

FILTER_MODE = $(filter $(addprefix modes/,$(1))/%,$(2))
CLEAN_MODE = $(info cleaning $(1)) \
	@$(RM) $(call FILTER_MODE,$(1),${MODE_ELC})
BUILD_MODE = $(info building $(mode)) \
	@$(call COMPILE,$(call FILTER_MODE,$(1),${MODE_EL}))
TESTS := $(wildcard test/*-tests.el)

.PHONY: test rebuild
all:
	${COMPILE} ${EL} ${MODE_EL}

%.elc: %.el
	${COMPILE} $^

rebuild: ## Recompile macs/subrs/and all base configs
	$(RM) $(CURDIR)/*.elc $(CURDIR)/macs/*.elc $(CURDIR)/subrs/*.elc
	$(MAKE)

.PHONY: clean-modes build-modes
clean-modes: ## Clean all modes
	$(foreach mode,${MODES},$(call CLEAN_MODE,$(mode)))

build-modes: ## Build all modes -- FIXME: does weird loopy shit
	$(foreach mode,${MODES},$(BUILD_MODE))

.PHONY: ${MODES}
clean-%: %   ## Clean mode
	$(call CLEAN_MODE,$^)

build-%: %   ## Build mode
	$(call CLEAN_MODE,$^)
	$(call BUILD_MODE,$^)

dep-%: %  ## print depends for package
	@grep "$^" .depend

test: ## Run tests
	$(BATCH) -l ert $(addprefix  -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

README.md : el2markdown.el ${PKG}.el ## Generate README.md from source
	$(BATCH) -l $< ${PKG}.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	wget \
  -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

.PHONY: unicode
unicode:  ## Generate latex/unicode abbrevs
	@julia ${BIN}/latex_abbrevs.jl abbrev nil ${LATEX_ABBREVS}

# .PHONY: .depend
.depend: $(EL) $(MODE_EL) ## create depends for *.el files
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

TAGS: ${EL} ${MODE_EL}
	$(RM) $@
	@touch $@
	ls $^ | xargs $(ETAGS) -a -o $@

.PHONY: clean distclean
clean:  ## clean temp files
	$(RM) *~ \#.*\#

distclean: clean-modes clean ## clean all generated files inc. compiled/auto
	$(RM) *loaddefs?.el *autoloads.el TAGS GPATH GTAGS \
		*.elc macs/*.elc subrs/*.elc

.PHONY: help
help:  ## Display this help message
	@for mfile in $(MAKEFILE_LIST); do                  \
	  grep -E '^[a-zA-Z_%-]+:.*?## .*$$' $$mfile |      \
	  sort | ${AWK}                                     \
	  'BEGIN {FS = ":.*?## "};                          \
	   {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'; \
	done
