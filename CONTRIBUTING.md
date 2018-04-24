# How to contribute

## Getting Started

*TBD*

## For developers

**NOTICE**: `phpactor.el` are under development.  These APIs are subject to change.

### Requirements

 * GNU Emacs 24.3+
 * PHP 7+
 * [Cask](https://github.com/cask/cask): Project management tool for Emacs
 * *(optional)* [Composer](https://getcomposer.org/): Dependency Manager for PHP

### Coding Rules

Please follow [the conventions of Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html).

 * [flycheck/flycheck](https://github.com/flycheck/flycheck): On the fly syntax checking for GNU Emacs
 * [purcell/flycheck-package](https://github.com/purcell/flycheck-package): Flycheck checker for elisp package metadata
 * [flycheck/flycheck-cask](https://github.com/flycheck/flycheck-cask): Cask support for Flycheck
 * [nameless](https://elpa.gnu.org/packages/nameless.html): Hide package namespace in your emacs-lisp code
   * The main developer is a fan of `nameless`.  We accept codes that do not follow this, but your indentation may be changed one day.
