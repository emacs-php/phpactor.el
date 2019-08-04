;;; phpactor-xref.el --- phpactor.el xref backend -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: Mikael Kermorgant <mikael@kgtech.fi>
;; Created: 05 Aug 2019
;; Version: 0.1.0
;; Keywords: tools, php
;; Package-Requires: ((phpactor "0.1.0")(seq "2"))
;; URL: https://github.com/emacs-php/phpactor.el
;; License: GPL-3.0-or-later

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
;; xref backend using Phpactor.
;; Inspiration comes from Nicolas Petton's https://github.com/NicolasPetton/xref-js2

;;; Code:
(require 'xref)
(require 'seq)

;;;###autoload
(defun phpactor-xref-backend ()
  "Xref-phpactor backend for Xref."
  'phpactor-xref)

(cl-defmethod xref-backend-references ((_backend (eql phpactor-xref)) symbol)
  (phpactor-xref--xref-find-references))

(cl-defmethod xref-backend-definitions ((_backend (eql phpactor-xref)) symbol)
  (phpactor-xref--find-definitions))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql phpactor-xref)))
  "Return a list of terms for completions taken from the symbols in the current buffer.
The current implementation returns all the words in the buffer,
which is really sub optimal."
  (list (symbol-at-point)))

(defun phpactor-xref--xref-find-references ()
  "Return a list of reference candidates."
  (seq-map (lambda (candidate)
             (phpactor-xref--make-xref candidate))
           (phpactor-xref--find-candidates)))

(defun phpactor-xref--find-definitions()
  "Return a list of candidates matching SYMBOL."
  (seq-map (lambda (candidate)
             (phpactor-xref--make-xref candidate))
           (phpactor-xref--find-candidates)))

(defun phpactor-xref--make-xref (candidate)
  "Return a new Xref object built from CANDIDATE."
  (xref-make (map-elt candidate 'match)
             (xref-make-file-location (map-elt candidate 'file)
                                      (map-elt candidate 'line)
                                      (map-elt candidate 'col))))

(defun phpactor-xref--find-candidates ()
  "Fetch references and return a list."
  (phpactor--find-references)
  (let ((current-references phpactor-references)
        matches)
    (dolist (file-reference current-references)
      (let ((path (plist-get file-reference :file)))
        (dolist (reference (plist-get file-reference :references))
          (when path
            (push  (list (cons 'file path)
                                 (cons 'line (plist-get reference :line_no))
                                 (cons 'col (plist-get reference :col_no))
                                 (cons 'match (plist-get reference :line))
                                        ;; (cons 'symbol symbol)
                                        ;; (cons 'match match)
                                 )
                   matches)))))
    matches))

(provide 'phpactor-xref)
;;; phpactor-xref.el ends here
