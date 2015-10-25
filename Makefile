EMACS ?= emacs
CASK ?= cask

all: install test

install:
	cask install

just-test:
	cask exec ert-runner

test: install just-test


.PHONY: test
