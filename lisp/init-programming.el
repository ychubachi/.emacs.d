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

;;;; diff-hl - diffをわかり易く表示
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1)
  :hook
  ((dired-mode . diff-hl-dired-mode)         ; Dired でも変更状態を表示
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)) ; Magit 操作後に即座に表示を同期
  :bind
  (("C-c v n" . diff-hl-next-hunk)           ; 次の変更箇所へジャンプ
   ("C-c v p" . diff-hl-previous-hunk)       ; 前の変更箇所へジャンプ
   ("C-c v d" . diff-hl-diff-goto-hunk)      ; 該当箇所の diff を開く
   ("C-c v r" . diff-hl-revert-hunk)         ; カーソル位置の変更を元に戻す
   ("C-c v s" . diff-hl-stage-current-hunk)) ; カーソル位置の変更のみをステージング
  :config
  ;; フリンジの見た目を少し太く・見やすく調整（お好みで）
  ;; (setq diff-hl-draw-borders nil)

  ;; フリンジがない環境（ターミナル等）や Flymake/Flycheck とフリンジが衝突する場合はマージン表示へ自動切替
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

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

;; テーマによって色が設定されていない場合がある
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; 各階層（1〜9）の色を明示的に指定する例
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#E06C75") ; 赤
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#98C379") ; 緑
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#E5C07B") ; 黄
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#61AFEF") ; 青
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#C678DD") ; 紫
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#56B6C2") ; シアン
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#D19A66") ; オレンジ
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#BE5046") ; 濃赤
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#ABB2BF") ; グレー
  ;; 不整合エラーの括弧を強調
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "#FFFFFF" :background "#E06C75" :weight 'bold))

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
