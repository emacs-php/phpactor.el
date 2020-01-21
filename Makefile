EMACS ?= emacs
CASK ?= cask
ELS = phpactor.el company-phpactor.el
AUTOLOADS = phpactor-autoloads.el
ELCS = $(ELS:.el=.elc)

%.elc: %.el
	$(EMACS) -Q -batch -L . --eval \
	"(let ((default-directory (expand-file-name \".cask\" default-directory))) \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path))" \
	-f package-initialize -f batch-byte-compile $<

all: .cask $(ELCS) autoloads

autoloads: $(AUTOLOADS)

$(AUTOLOADS): $(ELCS)
	$(EMACS) -Q -batch -L . --eval \
	"(progn \
	   (require 'package) \
	   (normal-top-level-add-subdirs-to-load-path) \
	   (package-generate-autoloads \"phpactor\" default-directory))"

.cask: Cask
	$(CASK) install

test: .cask $(ELCS)
	$(EMACS) -Q -batch -L . --eval \
	"(let ((default-directory (expand-file-name \".cask\" default-directory))) \
	   (normal-top-level-add-subdirs-to-load-path) \
           (require 'buttercup))" \
	--eval "(setq warning-minimum-log-level :debug)" \
	-f buttercup-run-discover

clean:
	rm -f $(ELCS) $(AUTOLOADS)

.PHONY: all autoloads clean test
