;;; init-core.el --- Emacs本体の基本設定  -*- lexical-binding: t; -*-
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

;; Emacsの組み込み機能の基本設定。

;;; Code:

;;; Emacsの組み込み機能を初期化する
(use-package emacs
  :ensure nil
;;;; custom
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

  ;; warnings
  (warning-suppress-types '((yasnippet backquote-change) (org-element-cache)))

  ;; GnuPG
  (epg-pinentry-mode 'loopback)
  (plstore-cache-passphrase-for-symmetric-encryption t)

  ;; user
  (user-full-name "Yoshihide Chubachi")
  (user-mail-address "yoshihide.chubachi@gmail.com")

;;;; bind
  :bind
  ("M-SPC" . cycle-spacing)

;;;; hook
  :hook
  (before-save . delete-trailing-whitespace)

;;;; init
  :init
  ;; (keyboard-translate ?\C-h ?\C-?)
  (global-set-key (kbd "C-h") #'delete-backward-char) ; C-hをBSにする
  (global-set-key (kbd "C-^") help-map) ; C-hの代わりにC-^をヘルプマップにする

  (defalias 'yes-or-no-p 'y-or-n-p) ; yos/noをy/nに変更する

  (ffap-bindings) ; ffap（ポイント位置のファイルを探す）を有効にする
  (global-goto-address-mode 1) ; バッファ内のすべてのURLやメールアドレスを自動でリンク化（クリック可能に）
  )

(provide 'init-core)
;;; init-core.el ends here
