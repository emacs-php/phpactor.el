;;; phpactor-action.el --- Phpactor editor actions   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 8 Apr 2018
;; Version: 0.0.1
;; Keywords: tools, php
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))
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

;; Phpactor RPC editor action implemetation.
;;
;; Following actions are defined:
;;
;;  - return
;;  - return_choice
;;  - echo
;;  - error
;;  - collection
;;  - open_file
;;  - close_file
;;  - file_references
;;  - input_callback
;;  - information
;;  - replace_file_source
;;
;; See https://phpactor.github.io/phpactor/rpc.html

;;; Code:
(require 'xref nil t)

;; Variables:
(defvar phpactor-action--message-format "Phpactor: %s")
(defvar phpactor-action--buffer-name "*Phpactor message*")

(defvar phpactor-action-table
  '((return . phpactor-action-return)
    (return_choice . phpactor-action-return-choice)
    (echo . phpactor-action-echo)
    (error . phpactor-action-error)
    (collection . phpactor-action-collection)
    (open_file . phpactor-action-open-file)
    (close_file . phpactor-action-close-file)
    (file_references . phpactor-action-file-references)
    (input_callback . phpactor-action-input-callback)
    (information . phpactor-action-information)
    (replace_file_source . phpactor-action-replace-file-source)))

;; Helper functions:
(defmacro phpactor-action--error (&rest args)
  "Signal an error, noticed from Phpactor by `ARGS'."
  (cons (if (fboundp 'user-error) #'user-error #'error)
        args))

;; Action functions:
(cl-defun phpactor-action-echo (&key message)
  "Echo message from Phpactor."
  (message phpactor-action--message-format message))

(cl-defun phpactor-action-information (&key message)
  "Pop information buffer from Phpactor by `ARGS'."
  (let ((buffer (get-buffer-create phpactor-action--buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert message))
    (pop-to-buffer buffer)))

(cl-defun phpactor-action-open-file (&key path offset)
  "Open file"
  (unless (and path offset)
    (phpactor-action--error "Definition not found"))

  (find-file path)
  (goto-char (1+ offset))
  (when (featurep 'xref)
    (xref-push-marker-stack)))

;; Dispatcher:
(cl-defun phpactor-action-dispatch (&key action parameters)
  "Execite action by `NAME' and `PARAMETERS'."
  (let ((func (cdr-safe (assq (intern action) phpactor-action-table))))
    (if func
        (apply func parameters)
      (error "Respond unknown/unimplemented action: %s" action))))

(provide 'phpactor-action)
;;; phpactor-action.el ends here
