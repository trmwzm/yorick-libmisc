MAKEFLAGS += --warn-undefined-variables
SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.DEFAULT_GOAL := all
.DELETE_ON_ERROR:
.SUFFIXES:

all: jk.txt

jk.txt:
	yx 'a= exp(1.0)' 'f= open("$@","w")' 'nb= write(f,a)' 'return nb==0'
