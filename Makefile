EMACS ?= emacs
CASK ?= cask

-include makefile-local

export EMACS

all: install test

install:
	cask install

just-test:
	cask exec ert-runner

test: install just-test


.PHONY: test
