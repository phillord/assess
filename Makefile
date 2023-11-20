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
	--eval '(assess-discover-run-and-exit-batch t)'

.PHONY: test dist

export:
	export


multi-test:
	make EMACS=$(EMACSES)/master/src/emacs test
	make EMACS=$(EMACSES)/emacs-26.1/src/emacs test
	make EMACS=$(EMACSES)/emacs-25.3/src/emacs test
	make EMACS=$(EMACSES)/emacs-24.5/src/emacs test
	make EMACS=$(EMACSES)/emacs-24.4/src/emacs test

elpa-sandbox:
	mkdir elpa-sandbox


cask-free-test: elpa-sandbox
	emacs --batch -q \
	--directory=. \
	--load test/local-sandbox.el \
	--eval '(assess-discover-run-and-exit-batch t)'

DOCKER_TAG=26
test-cp:
	docker run -it --rm --name docker-cp -v $(PWD):/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  silex/emacs:$(DOCKER_TAG)-ci-cask ./test-by-cp

test-git:
	docker run -it --rm --name docker-git -v $(PWD):/usr/src/app -w /usr/src/app --entrypoint=/bin/bash  silex/emacs:$(DOCKER_TAG)-ci-cask ./test-from-git


test-cp-29.1:
	$(MAKE) test-cp DOCKER_TAG=29.1

test-cp-28.2:
	$(MAKE) test-cp DOCKER_TAG=28.2

test-cp-27.2:
	$(MAKE) test-cp DOCKER_TAG=27.2

multi-test-cp: test-cp-29.1 test-cp-28.2 test-cp-27.2
	$(MAKE) test-cp DOCKER_TAG=26.2
	$(MAKE) test-cp DOCKER_TAG=26.1
	$(MAKE) test-cp DOCKER_TAG=25.3
	$(MAKE) test-cp DOCKER_TAG=25.2
	$(MAKE) test-cp DOCKER_TAG=25.1
	$(MAKE) test-cp DOCKER_TAG=master

multi-test-git:
	$(MAKE) test-git DOCKER_TAG=29.1
	$(MAKE) test-git DOCKER_TAG=28.2
	$(MAKE) test-git DOCKER_TAG=27.2
	$(MAKE) test-git DOCKER_TAG=26.3
	$(MAKE) test-git DOCKER_TAG=26.2
	$(MAKE) test-git DOCKER_TAG=26.1
	$(MAKE) test-git DOCKER_TAG=25.3
	$(MAKE) test-git DOCKER_TAG=25.2
	$(MAKE) test-git DOCKER_TAG=25.1
