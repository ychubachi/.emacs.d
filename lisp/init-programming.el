;;; init-programming.el --- Programming support -*- lexical-binding: t; -*-

;;; 一般
;;;; magit - Gitのフロントエンド
;; 内蔵されている古いtransientではなく、ELPA/MELPAの最新版を優先して読み込む
(use-package transient
  :defer t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)       ; 標準的なMagitの起動ショートカット
         ("C-x M-g" . magit-dispatch))  ; 各種Gitコマンドのポップアップ
  :config
  ;; 1. コミットメッセージ入力時に自動で折り返す（長文対策）
  (add-hook 'with-editor-mode-hook 'turn-on-auto-fill)

  ;; 2. 大規模リポジトリでの速度低下を防ぐ（Windows/Linux共通の高速化設定）
  (setq magit-refresh-status-buffer nil) ; バッファ切り替え時の自動更新を抑制

  ;; OSごとの個別最適化（Windows環境のMagitは遅くなりやすいため）
  (cond
   ((eq system-type 'windows-nt)
    ;; WindowsでMagitの挙動を高速化するハック
    (setq vc-handled-backends nil)    ; Emacs標準のVC機能を無効化してMagitに集中
    (setq magit-git-executable "git")) ; Gitのパスを明示（環境に応じてフルパスに）

   ((eq system-type 'gnu/linux)
    ;; Linux向けの特有設定があればここに記述
    nil)))

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
