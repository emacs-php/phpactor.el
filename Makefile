EMACS ?= emacs
CASK ?= cask
EASK ?= eask
ELCS = $(ELS:.el=.elc)

compile:
	$(EASK) compile

all: .eask $(ELCS) autoloads

autoloads:
	$(EASK) generate autoloads

.eask: Eask
	$(EASK) install-deps --dev

test: .eask $(ELCS)
	$(EASK) test buttercup

clean:
	$(EASK) clean all

.PHONY: all autoloads clean test
