wget  ?= wget
RM    = rm -rf
emacs ?= emacs

.PHONY: clean distclean nvp
all:
test: nvp
	$(emacs) -Q -batch --eval                             \
	'(progn (push "." load-path) (push "nvp" load-path))' \
	-L . -f batch-byte-compile-file nvp-clojure.el
	$(RM) nvp

.INTERMEDIATE: nvp
nvp:
	if [ ! -d nvp ]; then                                 \
	  git clone "https://github.com/nverno/nvp";          \
	fi

README.md: el2markdown.el nvp-clojure.el
	$(emacs) -batch -l $< nvp-clojure.el -f el2markdown-write-readme
	$(RM) $@~

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~

distclean: clean
	$(RM) *autoloads.el *loaddefs.el TAGS *.elc
