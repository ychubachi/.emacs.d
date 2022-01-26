;; emacs -q -l ~/.emacs.d/setup-package.el

;; Activate debugging.
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

(prog1 "package"
  (custom-set-variables
   '(package-archives '(("org"   . "https://orgmode.org/elpa/")
                        ("melpa" . "https://melpa.org/packages/")
                        ("gnu"   . "https://elpa.gnu.org/packages/"))))
  (package-initialize))

(prog1 "prepare leaf"
  (prog1 "package"
    (custom-set-variables
     '(package-archives
       '(("melpa" . "https://melpa.org/packages/")
         ("gnu"   . "https://elpa.gnu.org/packages/"))))
    (package-initialize))

  (prog1 "leaf"
    (unless (package-installed-p 'leaf)
      (unless (assoc 'leaf package-archive-contents)
        (package-refresh-contents))
      (condition-case err
          (package-install 'leaf)
        (error
         (package-refresh-contents)
         (package-install 'leaf)))))

  (leaf leaf
    :config
    (leaf leaf-keywords
      :ensure t
      :config (leaf-keywords-init))
    (leaf leaf-convert
      :ensure t)
    (leaf leaf-tree
      :ensure t
      :custom ((imenu-list-size . 30)
               (imenu-list-position . 'left)))
    (leaf hydra
      :ensure t)
    (leaf el-get
      :ensure t
      :custom ((el-get-git-shallow-clone  . t)))
    (leaf diminish
      :ensure t)))

(leaf straight
  :config
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (setq package-enable-at-startup nil))

(leaf org
  :straight (org :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"))
