;;; 最初期に実施するべき処理
;;;; no-littering - Emacsのバックアップファイルや一時ファイルをまとめる
(use-package no-littering
  :ensure (:wait t)
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))
  ;; Theme standard backups and undo-tree history locations
  (no-littering-theme-backups))

;;;; org - Orgモードの最新版を利用する
(use-package org
  :ensure (:wait t)  ;; Block until the updated Org package is ready
  )

;;; Emacs本体の設定
;;;; Emacsの組み込み変数を初期化する
(use-package emacs
  :ensure nil
  :custom
  ;; startup
  (inhibit-startup-screen t)

  ;; auto-revert
  (auto-revert-interval 1)      ; 再読み込みの間隔
  (auto-revert-verbose nil)     ; 再読込の際、メッセージを非表示
  (auto-revert-check-vc-info t) ; VCで更新があった場合、自動で更新

  ;; ui
  (ring-bell-function #'ignore)
  (line-spacing 0.25)

  ;; editing
  (fill-column 80)
  (indent-tabs-mode nil)
  (select-active-regions 'only)

  ;; byte-compile
  (byte-compile-warnings '(not cl-functions obsolete))

  ;; GnuPG
  (epg-pinentry-mode 'loopback)
  (plstore-cache-passphrase-for-symmetric-encryption t)

  ;; dired
  (dired-dwim-target t)

  ;; mail
  (user-full-name "Yoshihide Chubachi")
  (user-mail-address "yoshihide.chubachi@gmail.com")
  (mail-user-agent 'message-user-agent)
  (message-send-mail-function 'smtpmail-send-it)
  (smtpmail-stream-type 'starttls)
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)

  :bind
  ("M-SPC" . cycle-spacing)

  :hook
  (before-save . delete-trailing-whitespace)

  :config
  ;; TODO C-h -> Backspace
  ;; (keyboard-translate ?\C-h ?\C-?)

  ;; ffap
  (ffap-bindings)

  :init
  ;; (keyboard-translate ?\C-h ?\C-?)
  (global-set-key (kbd "C-h") #'delete-backward-char) ; C-hをBSにする
  (defalias 'yes-or-no-p 'y-or-n-p) ; yos/noをy/nに変更する
  (global-auto-revert-mode 1)
  )

;;;; undo-tree - C-zでUndoするようにする
(use-package undo-tree
  :demand t
  :bind ("C-z" . undo-tree-visualize)
  :config
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(provide 'init-core)
