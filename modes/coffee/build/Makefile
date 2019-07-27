emacs ?= emacs
python ?= python

ifeq ($(OS), Windows_NT)
  python = "C:/Program Files/Anaconda/python.exe"
endif

.PHONY: scrape clean
all: scrape toc clean

toc: $(json)
	$(emacs) -batch -l build.el -f build-toc

scrape :
	$(python) index.py

clean:
	$(RM) *.json *~ *.html
