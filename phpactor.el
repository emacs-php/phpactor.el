;;; phpactor.el --- Interface to Phpactor            -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@zonu.me>
;;         Mikael Kermorgant <mikael@kgtech.fi>
;; Created: 8 Apr 2018
;; Version: 0.1.0
;; Keywords: tools, php
;; Package-Requires: ((emacs "25.1") (f "0.17") (php-runtime "0.2") (composer "0.2.0") (async "1.9.3"))
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
(require 'f)
(require 'json)
(require 'php-project)
(require 'php-runtime)
(require 'ring)
(require 'subr-x)
(require 'composer)
(require 'async)
(require 'smart-jump nil t)

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

(defcustom phpactor-use-native-json t
  "If non-nil, use native json parsing if available."
  :group 'phpactor
  :type 'boolean)

;; Variables
(defvar phpactor--debug nil)
(defvar phpactor-history-size 100)
(defvar phpactor-history-ring nil)
(defvar phpactor-smart-jump-initialized nil)

(defvar phpactor--buffer-name "*Phpactor*")
(defvar phpactor-after-update-file-hook nil
  "Hook called after the file is updated by phpactor.")

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

(defun phpactor--find-executable ()
  "Return path to Phpactor executable file."
  (let ((vendor-executable (f-join phpactor-install-directory "vendor/bin/phpactor")))
    (if (file-exists-p vendor-executable)
        vendor-executable
      (warn "Phpactor not found.  Please run `phpactor-install-or-update' command")
      nil)))

(defcustom phpactor-executable (phpactor--find-executable)
  "Path to phpactor executable.
It is recommemded not to customize this, but if you do, you'll
have to ensure a compatible version of phpactor is used."
  :type 'string
  :safe #'stringp
  :group 'phpactor)

(defun phpactor-reset-executable (&rest _args)
  "Reset `phpactor-executable' variable by `phpactor--find-executable'."
  (prog1
      (setq phpactor-executable (phpactor--find-executable))
    (remove-hook 'compilation-finish-functions #'phpactor-reset-executable)))

(defun phpactor-ensure-executable ()
  "Ensure `phpactor' command installed."
  (interactive)
  (unless (and phpactor-executable (file-exists-p phpactor-executable))
    (phpactor-install-or-update)
    (phpactor-reset-executable)))

;; Utility functions

;;;###autoload
(defun phpactor-smart-jump-register (&optional modes)
  "Register `smart-jump' for MODES."
  (unless phpactor-smart-jump-initialized
    (smart-jump-register
     :modes (or modes '(php-mode phps-mode))
     :jump-fn 'phpactor-goto-definition
     :pop-fn 'pop-tag-mark
     :should-jump t
     :heuristic 'point
     :async t)
    (setq phpactor-smart-jump-initialized t)))

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
    (when (version< (php-runtime-expr "PHP_VERSION") "7.4.0")
      (setq directory (concat directory "/php73")))
    ;; Create .gitignore to prevent unnecessary files from being copied to GitHub
    (unless (file-exists-p (expand-file-name ".gitignore" phpactor-install-directory))
      (f-write-text "*\n" 'utf-8 (expand-file-name ".gitignore" phpactor-install-directory)))
    (cl-loop for file in '("composer.json" "composer.lock")
             for code = (format "copy(%s, %s)"
                                ;; Do not use `f-join' as this string may be a URL.
                                (php-runtime-quote-string (concat directory file))
                                (php-runtime-quote-string (concat phpactor-install-directory file)))
             do (php-runtime-expr code))
    (add-hook 'compilation-finish-functions 'phpactor-reset-executable)
    (composer nil "install" "--no-dev")))

(defalias 'phpactor-update #'phpactor-install-or-update)

(defun phpactor-get-working-dir ()
  "Return working directory of Phpactor."
  (directory-file-name
   (expand-file-name
    (or (php-project-get-root-dir) default-directory))))

(defun phpactor--expand-local-file-name (name)
  "Expand file name by `NAME'."
  ;; TODO: Support TRAMP
  (expand-file-name name))

(defun phpactor--make-command-string (sub-command &rest args)
  "Return command string by `SUB-COMMAND' and `ARGS'."
  (declare (indent 1))
  (mapconcat 'shell-quote-argument
             (cons phpactor-executable
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
  (let ((json (phpactor--serialize-json (list :action action
                                              :parameters arguments)))
        (output (get-buffer-create "*Phpactor Output*"))
        (coding-system-for-write 'utf-8)
        (cwd (phpactor-get-working-dir)))
    (with-current-buffer output (erase-buffer))
    (with-current-buffer (get-buffer-create "*Phpactor Input*")
      ;; `default-directory' is a *special variable* and buffer-local.
      (setq default-directory cwd)
      (erase-buffer)
      (insert json)
      (unless phpactor-executable
        (error "`phpactor-executable' is not set.  Please run `phpactor-install-or-update' command"))
      (call-process-region (point-min) (point-max) phpactor-executable nil output nil "rpc" (format "--working-dir=%s" default-directory))
      (phpactor--parse-json output))))

(defun phpactor--rpc-async (action arguments callback)
  "Async execute Phpactor `ACTION' subcommand with `ARGUMENTS' and calling `CALLBACK' after process."
  (declare (indent 2))
  (phpactor--add-history 'phpactor--rpc-async (list action arguments))
  (let* ((json (phpactor--serialize-json (list :action action
                                       :parameters arguments)))
         (coding-system-for-write 'utf-8)
         (default-directory (phpactor-get-working-dir))
         (executable phpactor-executable)
         (proc (async-start-process
                "phpactor-async" executable callback
                "rpc" (format "--working-dir=%s" default-directory))))
    (process-send-string proc json)
    (process-send-eof proc)))

(defun phpactor--parse-json (buffer)
  "Read JSON string from BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (if (and phpactor-use-native-json
             (fboundp 'json-serialize))
        (with-no-warnings
          (json-parse-buffer :object-type 'plist :array-type 'list))
      (let ((json-object-type 'plist) (json-array-type 'list))
        (json-read-object)))))

(defun phpactor--serialize-json (params)
  "Serialize PARAMS in to a JSON string."
  (if (and phpactor-use-native-json
           (fboundp 'json-serialize))
      (with-no-warnings
        (json-serialize params :null-object nil :false-object :json-false))
    (json-encode params)))

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
    (override_method . phpactor-override-method)
    (close_file . phpactor-action-close-file)
    (file_references . phpactor-action-file-references)
    (input_callback . phpactor-action-input-callback)
    (information . phpactor-action-information)
    (replace_file_source . phpactor-action-update-file-source)
    (update_file_source . phpactor-action-update-file-source)))

;; Helper functions:
(cl-defun phpactor--action-input-parameters (value-type &key default label choices type multi keyMap)
  "Request user input by VALUE-TYPE, DEFAULT, LABEL, CHOICES, TYPE, MULTI.  Unuse KEYMAP."
  (if multi
      (cl-loop for input = (phpactor--action-input-parameters-1
                            value-type default label choices type)
               until (string= input "")
               collect input)
    (phpactor--action-input-parameters-1 value-type default label choices type)))

(defun phpactor--action-input-parameters-1 (value-type default label choices type)
  "Inner function of `phpactor--action-input-parameters' with VALUE-TYPE, DEFAULT, LABEL, CHOICES and TYPE."
  (when (eq type :null)
    (setq type nil))
  (let ((use-dialog-box nil)
        (type (if type (intern type) value-type)))
    (cl-case type
      (confirm (if (y-or-n-p label) t
                 (error "Action cancelled")))
      (file (read-file-name label nil default))
      (text (read-string label default))
      ((choice list) (completing-read label
                                      (cl-loop for (_ v) on choices by #'cddr
                                               collect v)))
      (t (error "Unknown input type %s" type)))))

(defun phpactor-action--collect-inputs (inputs)
  "Request input by INPUTS and return alist which collected the variables."
  (cl-loop for i in inputs
           for name = (intern (concat ":" (plist-get i :name)))
           for type = (intern (plist-get i :type))
           for parameters = (plist-get i :parameters)
           append (list name (apply #'phpactor--action-input-parameters type parameters))))

(defun phpactor-action--fill-vars (parameters input-vars)
  "Fill variables PARAMETERS by INPUT-VARS."
  (cl-loop for (key value) on parameters by #'cddr
           when (or (null value) (eq :null value))
           do (setq parameters (plist-put parameters key (plist-get input-vars key))))
  (cl-loop for (key value) on input-vars by #'cddr
           unless (plist-member parameters key)
           do (setq parameters (plist-put parameters key (plist-get input-vars key))))
  parameters)

(defun phpactor--command-argments-1 (key)
  "Return argument by `KEY'."
  (cl-case key
    (:source (buffer-substring-no-properties
              (point-min) (point-max)))
    (:path (phpactor--expand-local-file-name buffer-file-name))
    (:source_path (phpactor--expand-local-file-name buffer-file-name))
    (:offset (1- (position-bytes (point))))
    (:offset_start (1- (position-bytes (region-beginning))))
    (:offset_end (1- (position-bytes (region-end))))
    (:current_path (phpactor--expand-local-file-name buffer-file-name))
    (t (error "`%s' is unknown argument" key))))

(defun phpactor--command-argments (&rest arg-keys)
  "Collect arguments by `ARG-KEYS'."
  (cl-loop for key in arg-keys
           for arg = (phpactor--command-argments-1 key)
           append (list key arg)))

(cl-defun phpactor-action-error (&key message details)
  "Echo error MESSAGE and DETAILS from Phpactor."
  (when phpactor--debug
    (phpactor-action-information :information message :details details))
  (user-error message))

;; Action functions:
(cl-defun phpactor-action-echo (&key message)
  "Echo MESSAGE from Phpactor."
  (message phpactor-action--message-format message))

(cl-defun phpactor-action-input-callback (&key callback inputs)
  "Require INPUTS and dispatch CALLBACK."
  (let* ((input-vars (phpactor-action--collect-inputs inputs))
         (parameters (phpactor-action--fill-vars (plist-get callback :parameters) input-vars)))
    (apply #'phpactor-action-dispatch (phpactor--rpc (plist-get callback :action) parameters))))

(cl-defun phpactor-action-information (&key information details)
  "Pop INFORMATION buffer with DETAILS from Phpactor."
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
  "Return VALUE from Phpactor."
  value)

(defvar phpactor-references nil)

(cl-defun phpactor-action-file-references (&key file_references)
  "Receives a list of FILE_REFERENCES for information purpose."
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

(defun phpactor-list-references ()
  "View references in a new buffer."
  (interactive)
  (let ((current-references phpactor-references))
    (switch-to-buffer (get-buffer-create phpactor-references-buffer))
    (set-window-dedicated-p (get-buffer-window) t)
    (setq buffer-read-only nil)
    (erase-buffer)
    (dolist (file-reference current-references)
      (let ((path (plist-get file-reference :file)))
        (dolist (reference (plist-get file-reference :references))
          (when path
            (insert-text-button (phpactor-truncate-left path phpactor-references-list-col1-width)
                                'action (lambda (_) (find-file path) (goto-char (plist-get reference :start)))
                                'help-echo "mouse-2: visit this file in other window")
            (insert ": ")
            (insert (number-to-string (plist-get reference :line_no)))
            (insert "\n")))))
    (goto-char 0)
    (grep-mode)))

(cl-defun phpactor-action-collection (&key actions)
  "Executes a collection of ACTIONS."
  (mapc
   (lambda (action)
     (apply #'phpactor-action-dispatch (list :action (plist-get action :name) :parameters (plist-get action :parameters))))
   actions))

(cl-defun phpactor-action-open-file (&key path offset force_reload target)
  "Open file from Phpactor."
  (unless (and path offset)
    (user-error "Definition not found"))
  (unless (file-name-absolute-p path)
    (setq path (expand-file-name path (phpactor-get-working-dir))))

  ;; TODO: Implement other target: Phpactor\Extension\Rpc\Response\OpenFileResponse
  ;; `target' expects "focused_window", "vsplit", "hsplit" and "new_tab"
  (unless target
    (setq target "focused_window"))

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
  "Close file by PATH from Phpactor."
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
            (error "Invalid rcs patch or internal error in phpactor--apply-rcs-patch"))
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
    (with-current-buffer (find-file-noselect path)
      (unwind-protect
          (save-restriction
            (widen)
            (with-current-buffer patchbuf
              (erase-buffer))
            (with-temp-file tmpfile
              (insert source))
            (if (zerop (call-process-region (point-min) (point-max)
                                            "diff" nil patchbuf nil "-n" "-" tmpfile))
                (message "Buffer was unchanged by phpactor")
              (phpactor--apply-rcs-patch patchbuf)
              (message "Buffer modified by phpactor")))
        (kill-buffer patchbuf)
        (delete-file tmpfile)))
    (run-hooks 'phpactor-after-update-file-hook)))

;; Dispatcher:
(cl-defun phpactor-action-dispatch (&key action parameters version)
  "Execite ACTION by PARAMETERS with VERSION."
  (when (and version (not (equal phpactor--supported-rpc-version version)))
    (if phpactor-executable
        (error "Phpactor uses rpc protocol %s but this package requires %s" version phpactor--supported-rpc-version)
      (user-error "Phpactor should be upgraded.  Please run `phpactor-install-or-update' command")))
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
  (apply #'phpactor-action-dispatch (phpactor--rpc "extension_list" [])))

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
(defun phpactor-import-class (&optional name)
  "Execute Phpactor RPC import_class command for class NAME.

If called interactively, treat current symbol under cursor as NAME.
If any region is active, it takes precedence over symbol at point."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :offset :path))
        (name (or name (if (region-active-p)
                           (buffer-substring (point) (mark))
                         (symbol-name (symbol-at-point))))))
    (apply #'phpactor-action-dispatch (phpactor--rpc "import_class" (append arguments (list :qualified_name name))))))

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
  "Execute Phpactor RPC extract_constant action."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path :offset)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "extract_constant" arguments))))

;;;###autoload
(defun phpactor-hover ()
  "Execute Phpactor RPC hover action."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :offset)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "hover" arguments))))

;;;###autoload
(defun phpactor-extract-method ()
  "Execute Phpactor RPC extract_method action."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path :offset_start :offset_end)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "extract_method" arguments))))

;;;###autoload
(defun phpactor-extract-expression ()
  "Execute Phpactor RPC extract_expression action."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path :offset_start :offset_end)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "extract_expression" arguments))))

;;;###autoload
(defun phpactor-change-visibility ()
  "Execute Phpactor RPC change_visibility action."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path :offset)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "change_visibility" arguments))))

;;;###autoload
(defun phpactor-override-method ()
  "Execute Phpactor RPC override_method action."
  (interactive)
  (let ((arguments (phpactor--command-argments :source :path)))
    (apply #'phpactor-action-dispatch (phpactor--rpc "override_method" arguments))))

(provide 'phpactor)
;;; phpactor.el ends here
