EMACS ?= emacs
CASK ?= cask

-include makefile-local

export EMACS

all: install test

install:
	EMACS=$(EMACS) cask install

just-test:
	EMACS=$(EMACS) cask exec ert-runner

test: install just-test

package:
	EMACS-$(EMACS) cask package

discover-test:
	EMACS=$(EMACS) cask exec $(EMACS) --batch -l sisyphus.el -l sisyphus-discover.el -f sisyphus-discover-run-and-exit-batch

.PHONY: test dist

export:
	export
