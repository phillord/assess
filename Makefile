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

package:
	cask package

discover-test:
	cask exec emacs --batch --load sisyphus.el --load sisyphus-discover.el \
	-f sisyphus-discover-run-and-exit-batch

.PHONY: test dist

