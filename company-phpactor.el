;;; company-phpactor.el --- company-mode backend for Phpactor -*- lexical-binding t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: Martin Tang <martin.tang365@gmail.com>
;; Created: 18 Apr 2018
;; Version: 0.0.2
;; Keywords: tools, php
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5") (company "0.9.6"))
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

;; Company integration for Phpactor.

;;; Code:
(require 'company)
(require 'phpactor)

(defun company-phpactor--grab-symbol ()
  "If point is at the end of a symbol, return it.
Otherwise, if point is not inside a symbol, return an empty string.
Here we create a temporary syntax table in order to add $ to symbols."
  (let (($temp-syn-table (make-syntax-table)))
    (modify-syntax-entry ?\$ "_" $temp-syn-table)

    (with-syntax-table $temp-syn-table
      (if (looking-at "\\_>")
          (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                    (point)))
        (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
          "")))))

(defun company-phpactor--get-suggestions ()
  "Get completions for current cursor."
  (let ((response (phpactor--rpc "complete" (phpactor--command-argments :source :offset))))
    (plist-get (plist-get (plist-get response  :parameters) :value) :suggestions)))

;;;###autoload
(defun company-phpactor (command &optional arg &rest ignored)
  "`company-mode' completion backend for Phpactor."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-phpactor))
    (prefix (company-phpactor--grab-symbol))
    (candidates (all-completions (substring-no-properties arg) (mapcar #'(lambda (suggestion) (plist-get suggestion :name)) (company-phpactor--get-suggestions))))))

(provide 'company-phpactor)
;;; company-phpactor.el ends here
