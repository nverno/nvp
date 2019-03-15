-include config.mk
include default.mk

.PHONY: test
all: test

%.elc: %.el
	@${COMPILE} $<

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
	@julia ${SCRIPT}/latex_abbrevs.jl abbrev nil ${LATEX_ABBREVS}

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

.PHONY: clean distclean
clean:  ## clean temp files
	$(RM) *~ \#.*\#

distclean: clean ## clean all generated files including compiled & autoloads
	$(RM) *loaddefs?.el *autoloads.el

.PHONY: help
help:  ## Show help for targets
	@grep -E '^[/.%0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort | ${AWK}                                           \
	'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
