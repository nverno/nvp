RM     = rm -rf
emacs  ?= emacs
wget   ?= wget
python ?= python

.PHONY: nvp clean
all: 

.INTERMEDIATE: nvp
nvp:
	if [ ! -d nvp ]; then                        \
	  git clone "https://github.com/nverno/nvp"; \
	fi

README.md : el2markdown.el python-tools.el
	$(emacs) -batch -l $< python-tools.el -f el2markdown-write-readme

.INTERMEDIATE: el2markdown.el
el2markdown.el:
	$(wget) -q -O $@ "https://github.com/Lindydancer/el2markdown/raw/master/el2markdown.el"

clean:
	$(RM) *~
