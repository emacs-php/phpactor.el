(package "phpactor"
         "0.1.0"
         "Interface to Phpactor")

(website-url "https://github.com/emacs-php/phpactor.el")
(keywords "tools" "php")

(package-file "phpactor.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'melpa)
(source 'gnu)

(depends-on "emacs" "25.1")
(depends-on "php-runtime")
(depends-on "composer")
(depends-on "async")

(development
 (depends-on "php-mode")
 (depends-on "buttercup"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
