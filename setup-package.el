;; emacs -l ~/.emacs.d/setup-package.el

(prog1 "package"
  (custom-set-variables
   '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                        ("melpa" . "https://melpa.org/packages/")
                        ("gnu"   . "https://elpa.gnu.org/packages/"))))
  (package-initialize))
