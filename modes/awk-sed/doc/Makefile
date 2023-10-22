SHELL = /bin/bash

all: builtins.el

builtins.el: builtins.ts
	./builtins.awk $^ > $@
