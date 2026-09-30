;;; init-programming.el --- プログラミング支援の設定  -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2026 Yoshihide Chubachi

;; Author: Yoshihide Chubachi <yoshi@chubachi.net>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; LSP・構文チェック・Lisp編集・プロジェクト管理など、プログラミング全般の設定。

;;; Code:

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
  :vc (:url "https://github.com/emacsmirror/paredit") ; 2026/09/14 本家のサイトにアクセスできない
  :commands (paredit-mode)
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

;;; その他
;;;; Dockerfile

(use-package dockerfile-mode
  :config
  (put 'dockerfile-image-name
       'safe-local-variable
       #'stringp))

;;;; yaml-mode - YAMLファイルの編集
(use-package yaml-mode
  :ensure t)

;;;; projectile - プロジェクト管理
;; cofu の バインディングと重なっていたため、cofu側を変更

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("C-c p s" . consult-ripgrep))
  :custom
  (projectile-project-search-path '("~/.emacs.d/" ("~/git" . 1)))
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  ;; ripgrep がインストールされている場合に優先使用
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --hidden --glob '!.git'"))
  )

;;;; consult-projectile
(use-package consult-projectile :ensure t :after projectile)

(provide 'init-programming)
;;; init-programming.el ends here
