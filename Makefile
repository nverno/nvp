emacs  ?= emacs
wget   ?= wget
SCRIPT = script

.PHONY: test
all: test
test:
	$(emacs) -Q -batch -L . -l ert -l test/nvp-tests.el \
	-f ert-run-tests-batch-and-exit

README.md : el2markdown.el nvp.el
	$(emacs) -batch -l $< nvp.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

.PHONY: unicode
unicode:
	@julia ${SCRIPT}/latex_abbrevs.jl abbrev nil unicode-latex-abbrev-table

clean:
	$(RM) *~ dist
