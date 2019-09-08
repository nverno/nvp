-include config.mk
include default.mk

MODEDIRS := $(shell find modes/ -mindepth 1 -maxdepth 1 -type d)
MODES := $(sort $(notdir ${MODEDIRS}))
MODE_EL := $(shell find modes/ -type f -name \*.el \
		\! \( -path \*/snippets/* -o -name .\* -o -path \*/unused/* \
		-o -path \*/test/\* -o -path \*/etc/\* -o -name \*w32\* \))
MODE_ELC := $(addsuffix c,${MODE_EL})

FILTER_MODE = $(filter $(addprefix modes/,$(1))/%,$(2))
CLEAN_MODE = $(info cleaning $(1)) \
	$(RM) $(call FILTER_MODE,$(1),${MODE_ELC})
BUILD_MODE = $(info building $(1)) \
	$(call COMPILE,$(call FILTER_MODE,$(1),${MODE_EL}))
MAP_MODES = $(foreach mode,${MODES},$(call $(1),$(mode)))

.PHONY: test
all: 

%.elc: %.el
	@${COMPILE} $<

# Manage modes
.PHONY: ${MODES}
clean-%: %
	$(call CLEAN_MODE,$^)

build-%: %
	$(call BUILD_MODE,$^)

.PHONY: clean-modes build-modes
clean-modes:
	$(call MAP_MODES,${CLEAN_MODE})

build-modes:
	$(call MAP_MODES,${BUILD_MODE})

test: ## Run tests
	$(BATCH) -l ert -l test/nvp-tests.el -f ert-run-tests-batch-and-exit

README.md : el2markdown.el ${PKG}.el ## Generate README.md from source
	$(BATCH) -l $< ${PKG}.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	wget \
  -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

.PHONY: unicode
unicode:  ## Generate latex/unicode abbrevs
	@julia ${BIN}/latex_abbrevs.jl abbrev nil ${LATEX_ABBREVS}

.depend: $(EL) ## create depends for package .el files
	$(info Computing depends)
	@rm -f .depend
	@for f in $(EL); do                                                  \
	    sed -n                                                           \
		"s/.*(require '\(${PKG}[^) ]*\).*).*$$/$${f}c: \1.elc/p" $$f \
		>> .depend;                                                  \
	done

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

TAGS: ${EL}
	$(RM) $@
	@touch $@
	ls $(EL) | xargs $(ETAGS) -a -o $@

.PHONY: clean distclean
clean:  ## clean temp files
	$(RM) *~ \#.*\#

distclean: clean ## clean all generated files including compiled & autoloads
	$(RM) *loaddefs?.el *autoloads.el TAGS GPATH GTAGS

.PHONY: help
help:  ## Show help for targets
	@grep -E '^[/.%0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort | ${AWK} \
	'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
