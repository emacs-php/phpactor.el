(require 'phpactor)

(defun buffer-string* (buffer)
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max)  )))

(describe "var: `phpactor-history-size'"
  (it "should have default value"
    (expect phpactor-history-size :to-be 100)
    ))

(describe "var: `phpactor-install-directory'"
  (it "should have some value"
    (display-warning 'buttercup (format "phpactor install folder is : %s" phpactor-install-directory))
    (expect phpactor-install-directory :not :to-be nil)
    ))

(describe ": `phpactor--lisp-directory'" ;; I prefer having a test as I'm not that really fine with it being nil
  (it "should have some value"
      (display-warning 'buttercup (format "phpactor lisp folder is : %s" phpactor--lisp-directory))
      (expect phpactor--lisp-directory :not :to-be nil)
))

(describe "defun: `phpactor-install-or-update'"
  (it "should find phpactor installed under phpactor-install-directory :"
    (let ((timeout-duration 300))
      (phpactor-install-or-update)
      (with-timeout
          (timeout-duration (display-warning 'buttercup (format "Error : timeout waiting %s seconds for composer install to finish" timeout-duration)))
        (while (not (file-exists-p (f-join phpactor-install-directory "vendor/bin/phpactor")))
          (sleep-for 1)))
      (expect (phpactor-find-executable) :to-equal (f-join phpactor-install-directory "vendor/bin/phpactor"))
      )))

(describe "defun: `phpactor-get-working-dir'"
  (it "should rely on php-project"
    (spy-on 'php-project-get-root-dir :and-call-through)
    ;; (display-warning 'buttercup (format "phpactor-working-dir is : %s" phpactor-working-dir))
    (with-current-buffer (find-file "tests/src/Book.php")
      ;; (message "phpactor-working-dir is : %s" (php-project-get-root-dir))
      (phpactor-get-working-dir)
      (expect 'php-project-get-root-dir :to-have-been-called)
      )
    ))
