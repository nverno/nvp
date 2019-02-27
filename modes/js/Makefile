emacs ?= emacs
wget  ?= wget

.PHONY: test
all:

README.md : el2markdown.el nvp-js.el
	$(emacs) -batch -l $< nvp-js.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~
