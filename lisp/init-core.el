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

;;; 最低限必要なEmacs本体の設定
;;;; C-hをBSにする
;; (keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-h") #'delete-backward-char)

;;;; yos/noをy/nに変更する
(defalias 'yes-or-no-p 'y-or-n-p)

;;; Emacs本体の設定
;;;; Emacsの組み込み変数を初期化する
(use-package emacs
  :ensure nil
  :custom
  ;; startup
  (inhibit-startup-screen t)

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

  ;; warnings TODO
  ;; (warning-suppress-types
  ;;  '(((yasnippet backquote-change))
  ;;    (org-element-cache)))

  :bind
  ("M-SPC" . cycle-spacing)

  :hook
  (before-save . delete-trailing-whitespace)

  :config
  ;; TODO C-h -> Backspace
  ;; (keyboard-translate ?\C-h ?\C-?)

  ;; ffap
  (ffap-bindings))

;;;; undo-tree - C-zでUndoするようにする
(use-package undo-tree
  :demand t
  :bind ("C-z" . undo-tree-visualize)
  :config
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(provide 'init-core)
