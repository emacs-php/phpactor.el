;;; test-sanity.el --- Behavior-Driven test for phpactor -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: Mikael Kermorgant <mikael@kgtech.fi>
;; Keywords: maint

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A BDD test code based on Buttercup.
;; https://github.com/jorgenschaefer/emacs-buttercup

;;; Code:
(require 'buttercup)
(require 'phpactor)

(describe "var: `phpactor-history-size'"
  (it "should have default value"
    (expect phpactor-history-size :to-be nil)))

(describe "var: `phpactor-install-directory'"
  (it "should have some value"
    (display-warning 'buttercup
                     (format "Phpactor installed directory is: %s" phpactor-install-directory) :debug)
    (expect phpactor-install-directory :not :to-be nil)))

(describe "var: `phpactor--lisp-directory'"
  ;; I prefer having a test as I'm not that really fine with it being nil
  (it "should have some value"
    (display-warning 'buttercup
                     (format "Phpactor Lisp directory is: %s" phpactor--lisp-directory) :debug)
    (expect phpactor--lisp-directory :not :to-be nil)))

(describe "defun: `phpactor-install-or-update'"
  (it "should find phpactor installed under phpactor-install-directory:"
    (let ((timeout-duration 300)
          (expected-phpactor-path
           (expand-file-name "vendor/bin/phpactor" phpactor-install-directory)))
      (phpactor-install-or-update)
      (with-timeout
          (timeout-duration
           (display-warning
            'buttercup
            (format "timeout waiting %s seconds for composer install to finish"
                    timeout-duration)
            :error))
        (while (not (file-exists-p expected-phpactor-path))
          (sleep-for 1))
        (sleep-for 1))
      (expect phpactor-executable :to-equal expected-phpactor-path))))

(describe "defun: `phpactor-get-working-dir'"
  (it "should rely on php-project"
    (spy-on 'php-project-get-root-dir :and-call-through)
    ;; (display-warning 'buttercup (format "phpactor-working-dir is : %s" phpactor-working-dir))
    (with-current-buffer (find-file "tests/src/Book.php")
      ;; (message "phpactor-working-dir is : %s" (php-project-get-root-dir))
      (phpactor-get-working-dir)
      (expect 'php-project-get-root-dir :to-have-been-called))))

(provide 'test-sanity)
;;; test-sanity.el ends here
