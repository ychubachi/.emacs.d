;;; recentf
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)

  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/elpaca"))

  :config
  (run-at-time nil (* 5 60)
               #'recentf-save-list))

;;; savehist
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;;; saveplace
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))

;;; auto-revert
(use-package autorevert
  :ensure nil

  :custom
  (auto-revert-interval 1)
  (auto-revert-verbose nil)

  :init
  (global-auto-revert-mode 1))

;;; files
(use-package files
  :ensure nil

  :custom
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)

  ;; シンボリックリンクを自動で辿る
  (vc-follow-symlinks t))

;;; vc-hooks
(use-package vc-hooks
  :ensure nil
  :custom
  (vc-handled-backends '(Git))) ; Gitのみ使用

;;; dired
(use-package dired
  :ensure nil

  :custom
  (dired-dwim-target t))

;;; wdired
(use-package wdired
  :ensure nil

  :bind
  (:map dired-mode-map
        ("r" . wdired-change-to-wdired-mode)))

;;; WSL環境でリンクをクリックした時に、Windows側のブラウザで開く設定
;; wslview (wsluパッケージ) が必要: sudo apt install wslu
(use-package emacs
  :ensure nil
  :config
  (defun cmd/wsl-browser (url &rest _ignore)
    "Browse URL using wslview."
    (interactive "sURL: ")
    (shell-command (concat "wslview " "'" url "'")))

  (when (and (eq system-type 'gnu/linux)
             (getenv "WSLENV"))
    (setq browse-url-browser-function 'cmd/wsl-browser)))

;;; Dired上で `J` を押すと、Windows側の既定のアプリ（Word、PDF、画像など）で開く
;; wslview (wsluパッケージ) が必要: sudo apt install wslu
(use-package dired-launch
  :ensure t
  :hook (dired-mode . dired-launch-mode)
  :config
  (when (and (eq system-type 'gnu/linux)
             (getenv "WSLENV"))
    (setq dired-launch-default-launcher '("wslview"))))

(provide 'init-files)
