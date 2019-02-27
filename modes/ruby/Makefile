RM    = rm -rf
emacs ?= emacs
wget  ?= wget
ruby  ?= ruby

.PHONY: test
all: test
test: nvp
	$(emacs) -Q -batch -L . -L nvp -l ert -l test/ruby-tests.el \
	-f ert-run-tests-batch-and-exit
	$(RM) nvp

.INTERMEDIATE: nvp
nvp:
	@if [ ! -d nvp ]; then                                      \
	  git clone --depth=1 "https://github.com/nverno/nvp";      \
	fi

README.md : el2markdown.el ruby-utils.el
	$(emacs) -batch -l $< ruby-utils.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
