;;; init-files.el --- ファイル操作の設定  -*- lexical-binding: t; -*-
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

;; ファイルの履歴・自動保存・再読込、dired、WSL連携の設定。

;;; Code:

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
  (auto-revert-check-vc-info t) ; VCで更新があった場合、自動で更新

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

;;; WSLVIEWはサポート終了？
;; ;;; WSL環境でリンクをクリックした時に、Windows側のブラウザで開く設定
;; ;; wslview (wsluパッケージ) が必要: sudo apt install wslu
;; (use-package emacs
;;   :ensure nil
;;   :config
;;   (defun cmd/wsl-browser (url &rest _ignore)
;;     "Browse URL using wslview."
;;     (interactive "sURL: ")
;;     (shell-command (concat "wslview " "'" url "'")))

;;   (when (and (eq system-type 'gnu/linux)
;;              (getenv "WSLENV"))
;;     (setq browse-url-browser-function 'cmd/wsl-browser)))

;; ;;; Dired上で `J` を押すと、Windows側の既定のアプリ（Word、PDF、画像など）で開く
;; ;; wslview (wsluパッケージ) が必要: sudo apt install wslu
;; (use-package dired-launch
;;   :ensure t
;;   :hook (dired-mode . dired-launch-mode)
;;   :config
;;   (when (and (eq system-type 'gnu/linux)
;;              (getenv "WSLENV"))
;;     (setq dired-launch-default-launcher '("wslview"))))

(provide 'init-files)

(provide 'init-files)
;;; init-files.el ends here
