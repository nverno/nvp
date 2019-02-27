curl ?= curl
emacs ?= emacs
wget ?= wget

.PHONY: test
all: test
test: nvp-log.el
	$(emacs) -Q -batch -L . -l ert -l test/yaml-indent-tests.el \
	-f ert-run-tests-batch-and-exit

.INTERMEDIATE: nvp-log.el
nvp-log.el:
	$(curl) -fsSkL --retry 9 --retry-delay 9 \
	"https://raw.githubusercontent.com/nverno/nvp/master/nvp-log.el" \
	-o nvp-log.el

README.md : el2markdown.el yaml-tools.el
	$(emacs) -batch -l $< yaml-tools.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~
