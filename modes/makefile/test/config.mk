export SHELL = /bin/bash

AWK ?= $(shell command -v gawk || printf awk)
SED ?= sed

ROOT := ${CURDIR}
