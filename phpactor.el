;;; phpactor.el --- Interface to Phpactor            -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;;         Mikael Kermorgant <mikael@kgtech.fi>
;; Created: 8 Apr 2018
;; Version: 0.1.0
;; Keywords: tools, php
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (f "0.17") (php-runtime "0.2") (composer "0.1"))
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

;; The following definitions from go-mode.el have been adapted :
;; (Author: Dominik Honnef, url: https://github.com/dominikh/go-mode.el)
;;
;; go--apply-rcs-patch go--goto-line go--delete-whole-line

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'php-project)
(require 'php-runtime)
(require 'ring)
(require 'subr-x)
(require 'composer)

;; Custom variables
;;;###autoload
(defgroup phpactor nil
  "PHP refactoring and introspection"
  :prefix "phpactor-"
  :group 'tools
  :group 'php)

;;;###autoload
(defcustom phpactor-install-directory
  (eval-when-compile
    (expand-file-name (locate-user-emacs-file "phpactor/")))
  "Directory for setup Phactor.  (default `~/.emacs.d/phpactor/')."
  :type 'directory)

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

(defvar phpactor--debug nil)
(defvar phpactor-history-size 100)
(defvar phpactor-history-ring nil)

(defvar phpactor--buffer-name "*Phpactor*")

;;; Constants
(defconst phpactor-command-name "phpactor")
(defconst phpactor--supported-rpc-version "1.0.0")
(defconst phpactor--lisp-directory
  (let ((byte-compiled-dir
         (eval-when-compile
           (when (and (boundp 'byte-compile-current-file) byte-compile-current-file)
             byte-compile-current-file)))
        lib-dir)
    (if (and byte-compiled-dir (file-directory-p byte-compiled-dir))
        (file-name-directory byte-compiled-dir)
      (setq lib-dir (file-name-directory (locate-library "phpactor.el")))
      (when (and lib-dir (file-directory-p lib-dir))
        (file-name-directory lib-dir))))
  "Path to phpactor.el installed directory.
Byte compilation information or `locate-library' function is referenced.

NOTE: If you can not acquire either of them when you run Emacs, you will get
the necessary files from the Web.")

(defconst phpactor--remote-composer-file-url-dir
  "https://raw.githubusercontent.com/emacs-php/phpactor.el/master/"
  "Path of the URL for getting the files for Phpactor.
Please be aware that this files refers to the latest version regardless of
version.  It is also affected by changes in the distribution URL structure
of GitHub.")

;; Special variables
(defvar phpactor--execute-async nil)

;; Utility functions
(defun phpactor-find-executable ()
  "Return Phpactor command or path to executable."
  (or (when phpactor-executable
        (php-project--eval-bootstrap-scripts phpactor-executable))
      (let ((vendor-executable (f-join phpactor-install-directory "vendor/bin/phpactor")))
        (when (file-exists-p vendor-executable)
          vendor-executable))
      (error "Phpactor not found.  Please run phpactor-update")))

;;;###autoload
(defun phpactor-install-or-update ()
  "Install or update phpactor inside phpactor.el's folder."
  (interactive)
  (let* ((default-directory phpactor-install-directory)
         (directory (if (and phpactor--lisp-directory
                             (file-directory-p phpactor--lisp-directory)
                             (file-exists-p (expand-file-name "composer.json" phpactor--lisp-directory))
                             (file-exists-p (expand-file-name "composer.lock" phpactor--lisp-directory)))
                        phpactor--lisp-directory
                      phpactor--remote-composer-file-url-dir)))
    (unless (file-directory-p phpactor-install-directory)
      (make-directory phpactor-install-directory))
    ;; Create .gitignore to prevent unnecessary files from being copied to GitHub
    (unless (file-exists-p (expand-file-name ".gitignore" phpactor-install-directory))
      (f-write-text "*\n" 'utf-8 (expand-file-name ".gitignore" phpactor-install-directory)))
    (cl-loop for file in '("composer.json" "composer.lock")
             for code = (format "copy(%s, %s)"
                                ;; Do not use `f-join' as this string may be a URL.
                                (php-runtime-quote-string (concat directory file))
                                (php-runtime-quote-string (concat phpactor-install-directory file)))
             do (php-runtime-expr code))
    (composer nil "install" "--no-dev")
    ;; (call-process "composer" nil t nil "install" "--no-dev")
    ))
(defalias 'phpactor-update #'phpactor-install-or-update)

(defun phpactor-get-working-dir ()
  "Return working directory of Phpactor."
  (directory-file-name
   (expand-file-name (php-project-get-root-dir))))

(defun phpactor--expand-local-file-name (name)
  "Expand file name by `NAME'."
  ;; TODO: Support TRAMP
  (expand-file-name name))

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

(defun phpactor-config:dump ()
  "Execute Phpactor `config:dump' sub command."
  (interactive)
  (let ((default-directory (phpactor-get-working-dir)))
    (funcall
     (if (called-interactively-p 'interactive)
         #'shell-command
       #'shell-command-to-string)
     (phpactor--make-command-string "config:dump"))))

;; Phpactor RPC
(defun phpactor--rpc (action arguments)
  "Execute Phpactor `ACTION' subcommand with `ARGUMENTS'."
  (phpactor--add-history 'phpactor--rpc (list action arguments))
  (let ((json (json-encode (list :action action
                                 :parameters arguments)))
        (json-object-type 'plist)
        (json-array-type 'list)
        (output (get-buffer-create "*Phpactor Output*"))
        (phpactor-executable (phpactor-find-executable))
        (cwd (phpactor-get-working-dir)))
    (with-current-buffer output (erase-buffer))
    (with-current-buffer (get-buffer-create "*Phpactor Input*")
      ;; `default-directory' is a *special variable* and buffer-local.
      (setq default-directory cwd)
      (erase-buffer)
      (insert json)
      (call-process-region (point-min) (point-max) phpactor-executable nil output nil "rpc" (format "--working-dir=%s" default-directory))
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
    (replace_file_source . phpactor-action-update-file-source)
    (update_file_source . phpactor-action-update-file-source)))

;; Helper functions:
(cl-defun phpactor--action-input-parameters (value-type &key default label choices type)
  "Request user input by parameters."
  (let ((use-dialog-box nil)
        (type (if type (intern type) value-type)))
    (cl-case type
      (confirm (if (y-or-n-p label) t
                 (error "Action cancelled")))
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
  (cl-loop for (key value) on input-vars by #'cddr
           do (message "key:%s value:%s input:%s"
                       key value (plist-get input-vars key))
           unless (plist-member parameters key)
           do (setq parameters (plist-put parameters key
                                          (plist-get input-vars key))))
  parameters)

(defun phpactor--command-argments-1 (key)
  "Return argument by `KEY'."
  (cl-case key
    (:source (buffer-substring-no-properties
              (point-min) (point-max)))
    (:path (phpactor--expand-local-file-name buffer-file-name))
    (:source_path (phpactor--expand-local-file-name buffer-file-name))
    (:offset (1- (position-bytes (point))))
    (:current_path (phpactor--expand-local-file-name buffer-file-name))
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

(cl-defun phpactor-action-return (&key value)
  "Return var from Phpactor."
  value)

(defvar phpactor-references nil)

(cl-defun phpactor-action-file-references (&key file_references)
  "Receives a list of file references for information purpose."
  (setq-local phpactor-references file_references)
  (message "Phpactor changed %d references(s), use phpactor-list-references to check them" (length file_references)))

;;; Listing references in this buffer
(defconst phpactor-references-buffer "*Phpactor references*"
  "The name of the buffer to list referenced files.")

(defconst phpactor-references-list-col1-width 60)

(defun phpactor-truncate-left (string width)
  "Truncate STRING to WIDTH starting from the end, prepending ..."
  (if (> (length string) width)
      (concat "..." (substring string (- 3 width)))
    string))

(defun phpactor-open-file-other-window (path offset)
  "Open PATH at OFFSET in a different window."
  (save-selected-window
    (when (buffer-live-p (get-file-buffer path))
      (switch-to-buffer-other-window (get-file-buffer path)))
    (phpactor-action-open-file :path path :offset offset)))

(defun phpactor-references-list-make-entry (file-reference index)
  "Return an entry for the tabulated list, for FILE-REFERENCE at INDEX."
  (let ((path (plist-get file-reference :file))
        (in-project-path (string-remove-prefix (phpactor-get-working-dir) (plist-get file-reference :file)))
        (references (car (plist-get file-reference :references))))
    (list index
          (vector (list
                   (phpactor-truncate-left in-project-path phpactor-references-list-col1-width)
                   . ('action
                      (lambda (_event) (phpactor-open-file-other-window path (plist-get references :start)))
                      'help-echo path))
                  (number-to-string (plist-get references :line_no))))))

;; adapted from flycheck-list-errors
(cl-defun phpactor-list-references ()
  (interactive)
  (let ((current-references phpactor-references))
    (with-current-buffer (get-buffer-create phpactor-references-buffer)
      (setq tabulated-list-format (vector `("File" ,phpactor-references-list-col1-width nil) '("Line" 12 nil :right-align t))
            tabulated-list-padding 2
            tabulated-list-entries (seq-map-indexed #'phpactor-references-list-make-entry current-references))
      (tabulated-list-mode)
      (tabulated-list-init-header)
      (tabulated-list-print t)
      (switch-to-buffer-other-window phpactor-references-buffer))))

(cl-defun phpactor-action-collection (&key actions)
  "Executes a collection of actions."
  (mapc
   (lambda (action)
     (apply #'phpactor-action-dispatch (list :action (plist-get action :name) :parameters (plist-get action :parameters))))
   actions))

(cl-defun phpactor-action-open-file (&key path offset force_reload)
  "Open file from Phpactor."
  (unless (and path offset)
    (user-error "Definition not found"))

  (if (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack)
    (with-no-warnings
      (ring-insert find-tag-marker-ring (point-marker))))

  (let ((buf (find-buffer-visiting path)))
    (when (and force_reload buf)
      (progn
        (set-buffer buf)
        (revert-buffer t t t))))
  (find-file path)
  (goto-char (1+ (byte-to-position (max 1 offset)))))

(cl-defun phpactor-action-close-file (&key path)
  "Close file from Phpactor."
  (kill-buffer (find-file-noselect path t)))

;; this function was adapted from go-mode
(defun phpactor--goto-line (line)
  "Goto line LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

;; this function was adapted from go-mode
(defun phpactor--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

;; this function was adapted from go-mode
(defun phpactor--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0)
        (column (current-column)))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "Invalid rcs patch or internal error in go--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (cl-decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (phpactor--goto-line (- from line-offset))
                (cl-incf line-offset len)
                (phpactor--delete-whole-line len)))
             (t
              (error "Invalid rcs patch or internal error in phpactor--apply-rcs-patch")))))))
    (move-to-column column)))

(cl-defun  phpactor-action-update-file-source (&key path source edits)
  "Replace the source code in the current file."
  (interactive)
  (let ((tmpfile (make-temp-file "phpactor" nil ".php"))
        (patchbuf (get-buffer-create "*Phpactor patch*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer patchbuf
            (erase-buffer))

          (with-temp-file tmpfile
            (insert source))

          (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
              (message "Buffer was unchanged by phpactor")
            (phpactor--apply-rcs-patch patchbuf)
            (message "Buffer modified by phpactor")))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))

;; Dispatcher:
(cl-defun phpactor-action-dispatch (&key action parameters version)
  "Execite action by `NAME' and `PARAMETERS'."
  (when (and version (not (equal phpactor--supported-rpc-version version)))
    (if (phpactor-find-executable)
        (error "Phpactor uses rpc protocol %s but this package requires %s" version phpactor--supported-rpc-version)
      (error "Phpactor should be upgraded.  Please run phpactor-update")))
  (phpactor--add-history 'phpactor-action-dispatch (list :action action :parameters parameters :version version))
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
;;  - extension_list
;;  - extension_remove
;;  - extension_install
;;
;; See https://phpactor.github.io/phpactor/rpc.html#phpactor-commands

;;;###autoload
(defun phpactor-open-rpc-documentation (command)
  "Open the official documentation for COMMAND."
  (interactive "sRPC Command: ")
  (let ((doc-uri (concat "https://github.com/phpactor/phpactor/blob/develop/doc/rpc.md#" command)))
    (browse-url doc-uri)))

;;;###autoload
(defun phpactor-copy-class ()
  "Execute Phpactor RPC copy_class command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source_path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "copy_class" arguments))))

;;;###autoload
(defun phpactor-move-class ()
  "Execute Phpactor RPC move_class command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source_path)))
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
(defun phpactor-context-menu ()
  "Execute Phpactor RPC context_menu command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :offset :current_path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "context_menu" arguments))))

;;;###autoload
(defun phpactor-navigate ()
  "Execute Phpactor RPC navigate command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source_path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "navigate" arguments))))

;;;###autoload
(defun phpactor-extension-list ()
  "Execute Phpactor RPC extension_list command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source_path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "extension_list" arguments))))


;;;###autoload
(defun phpactor-extension-remove ()
  "Execute Phpactor RPC extension_remove command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source_path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "extension_remove" arguments))))

;;;###autoload
(defun phpactor-extension-install ()
  "Execute Phpactor RPC extension_install command."
  (interactive)
  (let ((arguments (phpactor--command-argments :source_path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "extension_install" arguments))))

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

;;;###autoload
(defun phpactor-import-class (name)
  "Execute Phpactor RPC import_class command for class NAME."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :offset :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "import_class" (append arguments (list :name name))))))

;;;###autoload
(defun phpactor-complete-constructor ()
  "Execute Phpactor RPC transform command to complete_constructor."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "transform" (append arguments (list :transform "complete_constructor"))))))

;;;###autoload
(defun phpactor-rename-variable (&optional scope)
  "Execute Phpactor RPC action to rename variable in SCOPE."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path :offset)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "rename_variable" (append arguments (list :scope scope))))))

;;;###autoload
(defun phpactor-rename-variable-local ()
  "Execute Phpactor RPC action to rename variable locally."
  (interactive)
  (phpactor-rename-variable "local"))

;;;###autoload
(defun phpactor-rename-variable-file ()
  "Execute Phpactor RPC action to rename variable in whole file."
  (interactive)
  (phpactor-rename-variable "file"))

(defun phpactor-complete-properties ()
  "Execute Phpactor RPC transform command to add_missing_properties."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "transform" (append arguments (list :transform "add_missing_properties"))))))

;;;###autoload
(defun phpactor-fix-namespace ()
  "Execute Phpactor RPC transform command to fix namespace."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "transform" (append arguments (list :transform "fix_namespace_class_name"))))))

;;;###autoload
(defun phpactor-implement-contracts ()
  "Execute Phpactor RPC transform command to implement contracts."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "transform" (append arguments (list :transform "implement_contracts"))))))

;;;###autoload
(defun phpactor-find-references ()
  "Execute Phpactor RPC references action to find references."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path :offset)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "references" arguments))
    (phpactor-list-references)))

;;;###autoload
(defun phpactor-replace-references ()
  "Execute Phpactor RPC references action command to replace references."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path :offset)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "references" (append arguments (list :mode "replace"))))))

;;;###autoload
(defun phpactor-file-information ()
  "Execute Phpactor RPC file_info command to gather file informations."
  (interactive)
  (let ((arguments (phpactor--command-argments :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "file_info" arguments))))

;;;###autoload
(defun phpactor-insert-namespace ()
  "Find namespace for current file."
  (interactive)
  (let ((file-info (phpactor-file-information)))
    (insert (plist-get file-info :class_namespace))))

;;;###autoload
(defun phpactor-generate-accessors ()
  "Execute Phpactor RPC generate_accessor action."
  (interactive)
  (let ((arguments (phpactor--command-argments :path :offset :source)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "generate_accessor" arguments))))

;;;###autoload
(defun phpactor-generate-method ()
  "Execute Phpactor RPC generate_method action."
  (interactive)
  (let ((arguments (phpactor--command-argments :path :offset :source)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "generate_method" arguments))))

;;;###autoload
(defun phpactor-add-missing-assignments ()
  "Execute Phpactor RPC add_missing_assignments action."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "transform" (append arguments (list :transform "add_missing_properties"))))))

;;;###autoload
(defun phpactor-create-new-class ()
  "Execute Phpactor RPC class_new action."
  (interactive)
  (let ((arguments (phpactor--command-argments :current_path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "class_new" arguments))))

;;;###autoload
(defun phpactor-inflect-class ()
  "Execute Phpactor RPC class_inflect action."
  (interactive)
  (let ((arguments (phpactor--command-argments :current_path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "class_inflect" arguments))))

;;;###autoload
(defun phpactor-extract-constant ()
  "Execute Phpactor RPC extract-constant action."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :offset :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "extract_constant" arguments))))

(provide 'phpactor)
;;; phpactor.el ends here
