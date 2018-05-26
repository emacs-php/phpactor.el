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
;;
;; ## Instalation
;;
;; Please be aware that Phpactor is also in development stage.
;; A simple installation method is not yet provided, but you can build it using Composer.
;;
;; ## Configuration
;;
;; See https://phpactor.github.io/phpactor/configuration.html
;;

;;; Code:
(require 'json)
(require 'php-project)
(require 'ring)

;; Variables
;;;###autoload
(progn
  (defvar phpactor-executable nil
    "Path to `phpactor' executable file.")
  (make-variable-buffer-local 'phpactor-executable)
  (put 'phpactor-executable 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (or (null v) (stringp v))))))
;;;###autoload
(progn
  (defvar phpactor-working-dir nil
    "Path to working directory for Phpactor.")
  (make-variable-buffer-local 'phpactor-working-dir)
  (put 'phpactor-executable 'safe-local-variable
       #'(lambda (v) (if (consp v)
                         (and (eq 'root (car v)) (stringp (cdr v)))
                       (or (null v) (stringp v))))))

(defvar phpactor--debug nil)
(defvar phpactor-history-size 100)
(defvar phpactor-history-ring nil)

(defvar phpactor--buffer-name "*Phpactor*")

(defconst phpactor-command-name "phpactor")

;; Special variables
(defvar phpactor--execute-async nil)

;; Utility functions
(defun phpactor-find-executable ()
  "Return Phpactor command or path to executable."
  (or phpactor-executable
      (executable-find phpactor-command-name)))

(defun phpactor-get-working-dir ()
  "Return working directory of Phpactor."
  (directory-file-name
   (expand-file-name
    (or phpactor-working-dir (php-project-get-root-dir)))))

(defun phpactor--make-command-string (sub-command &rest args)
  "Return command string by `SUB-COMMAND' and `ARGS'."
  (declare (indent 1))
  (mapconcat 'shell-quote-argument
             (cons (phpactor-find-executable)
                   (cons sub-command args))
             " "))

(defun phpactor--add-history (name entry)
  "Add Phpactor history by `NAME' and `ENTRY'."
  (unless phpactor-history-ring
    (setq phpactor-history-ring (make-ring phpactor-history-size)))
  (ring-insert phpactor-history-ring (cons name entry)))


;; Phpactor RPC
(defun phpactor--rpc (action arguments)
  "Execute Phpactor `ACTION' subcommand with `ARGUMENTS'."
  (phpactor--add-history 'phpactor--rpc (list :action action :parameters arguments))
  (let ((json (json-encode (list :action action
                                 :parameters arguments)))
        (cmd  (phpactor--make-command-string "rpc"
                (format "--working-dir=%s" (phpactor-get-working-dir))))
        (json-object-type 'plist)
        (json-array-type 'list)
        (output (get-buffer-create "*Phpactor Output*")))
    (with-current-buffer (get-buffer-create "*Phpactor Input*")
      (erase-buffer)
      (insert json)
      (shell-command-on-region (point-min) (point-max) cmd output)
      (with-current-buffer output
        (goto-char (point-min))
        (json-read-object)))))

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
;; See https://phpactor.github.io/phpactor/rpc.html#editor-actions

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
(cl-defun phpactor--action-input-parameters (type &key default label choices)
  "Request user input by parameters."
  (let ((use-dialog-box nil))
    (cl-case type
      (file (read-file-name label nil default))
      (text (read-string label default))
      (choice (completing-read label
                               (cl-loop for (_ v) on choices by #'cddr
                                        collect v)))
      (t (error "Unknown input type %s" type)))))

(defun phpactor-action--collect-inputs (inputs)
  "Request input by `INPUTS' and return alist which collected the variables."
  (cl-loop for i in inputs
           for name = (intern (concat ":" (plist-get i :name)))
           for type = (intern (plist-get i :type))
           for parameters = (plist-get i :parameters)
           append (list name (apply #'phpactor--action-input-parameters type parameters))))

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

(defun phpactor--command-argments-1 (key)
  "Return argument by `KEY'."
  (cl-case key
    (:source (buffer-substring-no-properties
              (point-min) (point-max)))
    (:path buffer-file-name)
    (:offset (1- (point)))
    (t (error "`%s' is unknown argument" key))))

(defun phpactor--command-argments (&rest arg-keys)
  "Collect arguments by `ARG-KEYS'."
  (cl-loop for key in arg-keys
           for arg = (phpactor--command-argments-1 key)
           append (list key arg)))

(cl-defun phpactor-action-error (&key message details)
  "Echo error message from Phpactor."
  (when phpactor--debug
    (phpactor-action-information :information message :details details))
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

(cl-defun phpactor-action-information (&key information details)
  "Pop information buffer from Phpactor."
  (let ((buffer (get-buffer-create phpactor-action--buffer-name)))
    (with-current-buffer buffer
      (view-mode -1)
      (erase-buffer)
      (insert information)
      (when details
        (insert "\n\n")
        (insert details))
      (view-mode 1))
    (pop-to-buffer buffer)))

(cl-defun phpactor-action-open-file (&key path offset)
  "Open file from Phpactor."
  (unless (and path offset)
    (user-error "Definition not found"))

  (if (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack)
    (with-no-warnings
      (ring-insert find-tag-marker-ring (point-marker))))

  (find-file path)
  (goto-char (1+ offset)))

(cl-defun phpactor-action-replace-file-source (&key path source)
  "Replace the source code in the current file."
  (save-window-excursion
    (with-current-buffer (find-file-noselect path)
      ;; This is a simple implementation, so points will not be saved.
      ;; Should I copy the implementation of gofmt?  Umm...
      (erase-buffer)
      (insert source))))

;; Dispatcher:
(cl-defun phpactor-action-dispatch (&key action parameters)
  "Execite action by `NAME' and `PARAMETERS'."
  (phpactor--add-history 'phpactor-action-dispatch (list :action action :parameters parameters))
  (let ((func (cdr-safe (assq (intern action) phpactor-action-table))))
    (if func
        (apply func parameters)
      (error "Respond unknown/unimplemented action: %s" action))))

;; Phpactor commands

;; Phpactor RPC command implemetation.
;;
;; Following commands are defined:
;;
;;  - complete
;;  - class_search
;;  - goto_definition
;;  - copy_class
;;  - move_class
;;  - offset_info
;;  - transform
;;  - class_new
;;  - class_inflect
;;  - references
;;  - extract_constant
;;  - generate_method
;;  - generate_accessor
;;  - context_menu
;;  - navigate
;;
;; See https://phpactor.github.io/phpactor/rpc.html#phpactor-commands

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
(defun phpactor-offset-info ()
  "Execute Phpactor RPC offset_info command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :offset)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "offset_info" arguments))))

;;;###autoload
(defun phpactor-transform ()
  "Execute Phpactor RPC transform command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "transform" arguments))))

;;;###autoload
(defun phpactor-navigate ()
  "Execute Phpactor RPC navigate command."
  (interactive)
  (let ((arguments (list :source_path (file-relative-name buffer-file-name
                                                          (phpactor-get-working-dir)))))
    (apply #'phpactor-action-dispatch (phpactor--rpc "navigate" arguments))))

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
  (let ((arguments (phpactor--command-argments :source :offset :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "goto_definition" arguments))))

(provide 'phpactor)
;;; phpactor.el ends here
