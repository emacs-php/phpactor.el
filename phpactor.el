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

(defvar phpactor--debug nil)

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

;; Phpactor RPC
(defun phpactor--rpc (action arguments)
  "Execute Phpactor `ACTION' subcommand with `ARGUMENTS'."
  (let ((json (json-encode (list :action action
                                 :parameters arguments)))
        (cmd  (phpactor--make-command-string "rpc"
                (format "--working-dir=%s" (phpactor-get-working-dir))))
        (json-object-type 'plist)
        (json-array-type 'list))
    (with-current-buffer (get-buffer-create "*Phpactor Output*")
      (erase-buffer)
      (insert json)
      (when phpactor--debug
        (message "Phpactor RPC input: %s" (buffer-substring-no-properties
                                           (point-min) (point-max))))
      (shell-command-on-region (point-min) (point-max) cmd (current-buffer) t)
      (goto-char (point-min))
      (json-read-object))))

;;; Phpactor Action

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
(defmacro phpactor-action-error (&rest args)
  "Signal an error, noticed from Phpactor by `ARGS'."
  (cons (if (fboundp 'user-error) #'user-error #'error)
        args))

(cl-defun phpactor-action--input-parameters (&key default label type)
  "Request user input by parameters."
  (let ((use-dialog-box nil))
    (cl-case (if type (intern type) 'text)
      (file (read-file-name label nil default))
      (text (read-string label default))
      (t (error "Unknown input type %s" type)))))

(defun phpactor-action--collect-inputs (inputs)
  "Request input by `INPUTS' and return alist which collected the variables."
  (cl-loop for i in inputs
           for name = (intern (concat ":" (plist-get i :name)))
           for parameters = (plist-get i :parameters)
           append (list name (apply #'phpactor-action--input-parameters parameters))))

(defun phpactor-action--fill-vars (parameters input-vars)
  "Fill variables `PARAMETERS' by `INPUT-VARS'."
  (message "fill-vars %s %s" parameters input-vars)
  (cl-loop for (key value) on parameters by #'cddr
           do (message "key:%s value:%s input:%s"
                       key value (plist-get input-vars key))
           unless value
           do (setq parameters (plist-put parameters key
                                          (plist-get input-vars key))))
  parameters)

(cl-defun phpactor-action-error (&key message details)
  "Echo error message from Phpactor."
  (when phpactor--debug
    (phpactor-action-information :message message :details details))
  (user-error message))

;; Action functions:
(cl-defun phpactor-action-echo (&key message)
  "Echo message from Phpactor."
  (message phpactor-action--message-format message))

(cl-defun phpactor-action-input-callback (&key callback inputs)
  "Require `INPUTS' and dispatch `CALLBACK'."
  (let* ((input-vars (phpactor-action--collect-inputs inputs))
         (parameters (phpactor-action--fill-vars (plist-get callback :parameters) input-vars)))
    (message "%s" callback)
    (apply #'phpactor-action-dispatch (phpactor--rpc (plist-get callback :action) parameters))))

(cl-defun phpactor-action-information (&key message details)
  "Pop information buffer from Phpactor."
  (let ((buffer (get-buffer-create phpactor-action--buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert message)
      (when details
        (insert "\n\n")
        (insert details)))
    (pop-to-buffer buffer)))

(cl-defun phpactor-action-open-file (&key path offset)
  "Open file from Phpactor."
  (unless (and path offset)
    (phpactor-action--error "Definition not found"))

  (if (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack)
    (with-no-warnings
      (ring-insert find-tag-marker-ring (point-marker))))

  (find-file path)
  (goto-char (1+ offset)))

;; Dispatcher:
(cl-defun phpactor-action-dispatch (&key action parameters)
  "Execite action by `NAME' and `PARAMETERS'."
  (when phpactor--debug
    (message "Phpactor dispatch %s %s" action parameters))
  (let ((func (cdr-safe (assq (intern action) phpactor-action-table))))
    (if func
        (apply func parameters)
      (error "Respond unknown/unimplemented action: %s" action))))

;; Phpactor commands

;;;###autoload
(defun phpactor-copy-class ()
  "Execute Phpactor RPC copy_class command."
  (interactive)
  (let ((arguments (list :source_path (expand-file-name buffer-file-name))))
    (apply #'phpactor-action-dispatch (phpactor--rpc "copy_class" arguments))))

;;;###autoload
(defun phpactor-move-class ()
  "Execute Phpactor RPC move_class command."
  (interactive)
  (let ((arguments (list :source_path (expand-file-name buffer-file-name))))
    (apply #'phpactor-action-dispatch (phpactor--rpc "move_class" arguments))))

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
