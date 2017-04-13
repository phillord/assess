EMACS ?= emacs
CASK ?= cask

-include makefile-local

ifdef EMACS
EMACS_ENV=EMACS=$(EMACS)
endif

all: install test

install:
	$(EMACS_ENV) $(CASK) install

test: install just-test

package:
	$(EMACS_ENV) $(CASK) package

just-test:
	$(EMACS_ENV) $(CASK) emacs --batch -q \
	--directory=. \
	--load assess-discover.el \
	--funcall assess-discover-run-and-exit-batch

.PHONY: test dist

export:
	export

multi-test:
	make EMACS=$(EMACSES)/master/src/emacs test
	make EMACS=$(EMACSES)/emacs-25/src/emacs test
	make EMACS=$(EMACSES)/emacs-25.1/src/emacs test
	make EMACS=$(EMACSES)/emacs-24.5/src/emacs test
	make EMACS=$(EMACSES)/emacs-24.4/src/emacs test
	make EMACS=$(EMACSES)/emacs-24.3/src/emacs test
	make EMACS=$(EMACSES)/emacs-24.2/src/emacs test
	make EMACS=$(EMACSES)/emacs-24.1/src/emacs test
