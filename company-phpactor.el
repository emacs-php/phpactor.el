;;; company-phpactor.el --- company-mode backend for Phpactor -*- lexical-binding t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: Martin Tang <martin.tang365@gmail.com>
;; Created: 18 Apr 2018
;; Version: 0.0.1
;; Keywords: tools, php
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (company "0.9.6"))
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

(defun company-phpactor (command &optional arg &rest ignored)
  "`company-mode' completion backend for Phpactor."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-phpactor))
    (prefix (company-grab-symbol))
    (candidates (let* ((offset (point))
                       (response (phpactor--rpc "complete" (list :source (buffer-substring 1 offset) :offset offset))))
                  (mapcar (lambda (x) (plist-get x :name)) (plist-get (plist-get (plist-get response :parameters) :value) :suggestions))))))

(provide 'company-phpactor)
;;; company-phpactor.el ends here
