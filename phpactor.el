;;; phpactor.el --- Interface to Phpactor            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 8 Apr 2018
;; Version: 0.0.1
;; Keywords: tools, php
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
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

;; Phpactor is an intelligent code-completion and refactoring tool for PHP.
;; https://github.com/phpactor/phpactor

;;; Code:
(require 'json)
(require 'phpactor-action)
(require 'php-project)

;; Variables
;;;###autoload
(progn
  (defvar phpactor-executable nil
    "Path to `phpactor' executable file.")
  (make-variable-buffer-local 'phpactor-executable)
  (put 'phpactor-executable 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (null v) (stringp v)))))
;;;###autoload
(progn
  (defvar phpactor-working-dir nil
    "Path to working directory for Phpactor.")
  (make-variable-buffer-local 'phpactor-working-dir)
  (put 'phpactor-executable 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (null v) (stringp v)))))

(defvar phpactor--buffer-name "*Phpactor*")

(defconst phpactor-command-name "phpactor")

;; Special variables
(defvar phpactor--execute-async nil)

(defun phpactor-find-executable ()
  "Return Phpactor command or path to executable."
  (or phpactor-executable
      (executable-find phpactor-command-name)))

(defun phpactor-get-working-dir ()
  "Return working directory of Phpactor."
  (expand-file-name
   (or phpactor-working-dir (php-project-get-root-dir))))

(defun phpactor--make-command-string (sub-command &rest args)
  "Return command string by `SUB-COMMAND' and `ARGS'."
  (declare (indent 1))
  (mapconcat 'shell-quote-argument
             (cons (phpactor-find-executable)
                   (cons sub-command args))
             " "))

(defun phpactor--rpc (action arguments)
  "Execute Phpactor `ACTION' subcommand with `ARGUMENTS'."
  (let ((json (json-encode (list :action action
                                 :parameters arguments)))
        (cmd  (phpactor--make-command-string "rpc"
                (format "--working-dir=%s" (phpactor-get-working-dir))))
        (json-object-type 'plist)
        (json-array-type 'list))
    (with-current-buffer (get-buffer-create " *Phpactor Output*")
      (erase-buffer)
      (insert json)
      (shell-command-on-region (point-min) (point-max) cmd (current-buffer) t)
      (goto-char (point-min))
      (json-read-object))))

;;;###autoload
(defun phpactor-echo (message)
  "Execute Phpactor RPC echo command, say `MESSAGE'."
  (interactive "MInput Message: ")
  (let ((phpactor-action--message-format "Message from Phpactor: %s"))
    (apply #'phpactor-action-dispatch (phpactor--rpc "echo" (list :message message)))))

;;;###autoload
(defun phpactor-status ()
  "Execute Phpactor RPC status command, and pop to buffer."
  (interactive)
  (apply #'phpactor-action-dispatch (phpactor--rpc "status" [])))

;;;###autoload
(defun phpactor-goto-definition ()
  "Execute Phpactor RPC goto_definition command."
  (interactive)
  (let ((arguments (list :source (buffer-substring-no-properties
                                  (point-min) (point-max))
                         :offset (1- (point))
                         :path   buffer-file-name)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "goto_definition" arguments))))

(provide 'phpactor)
;;; phpactor.el ends here
