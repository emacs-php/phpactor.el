EMACS ?= emacs
ELS = phpactor.el company-phpactor.el
AUTOLOADS = phpactor-autoloads.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . -f package-initialize -f batch-byte-compile $<

all: clean autoloads $(ELCS)

autoloads: $(AUTOLOADS)

$(AUTOLOADS): $(ELCS)
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path) \
	   (package-generate-autoloads \"phpactor\" default-directory))"

clean:
	rm -f $(ELCS) $(AUTOLOADS)

.PHONY: all autoloads clean test
