;;; init-programming.el --- Programming support -*- lexical-binding: t; -*-

;;; 一般
;;;; Syntax check

(use-package flymake
  :ensure nil
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

;;;; LSP

(use-package eglot
  :ensure nil

  :hook
  ((python-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (typescript-mode . eglot-ensure))

  :custom
  (eglot-autoshutdown t))

;;;; インデントガイド

(use-package highlight-indent-guides
  :hook
  ((prog-mode . highlight-indent-guides-mode)
   (yaml-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'column))

;;; Lisp編集

;;;; カッコの対応関係
;; M-sがconsultの検索のデフォルトプリフィックスと重なるのでconsult側で対応
(use-package paredit
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (scheme-mode . enable-paredit-mode)))

;;;; 括弧を色分け

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;;; マクロ展開

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)))

;;;; ERT

(use-package ert
  :ensure nil
  :bind
  (("C-c t" . cmd/run-ert))

  :config
  (defun cmd/run-ert ()
    (interactive)
    (eval-buffer)
    (call-interactively #'ert)))

;;; その他
;;;; Dockerfile

(use-package dockerfile-mode
  :config
  (put 'dockerfile-image-name
       'safe-local-variable
       #'stringp))

(provide 'init-programming)
