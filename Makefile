EMACS = emacs
CASK ?= cask

#-include makefile-local

ifdef EMACS
EMACS_ENV=EMACS=$(EMACS)
endif


all: install test

install:
	$(EMACS_ENV) cask install

just-test:
	EMACS=$(EMACS) cask exec ert-runner

test: install just-test

package:
	EMACS-$(EMACS) cask package

discover-test:
	EMACS=$(EMACS) cask exec $(EMACS) --batch -l assess.el -l assess-discover.el -f assess-discover-run-and-exit-batch

.PHONY: test dist

export:
	export
