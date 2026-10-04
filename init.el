;;; init.el --- My init.el  -*- lexical-binding: t; -*-
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

;; lisp/
;; ├── init-package.el     ; elpaca, no-littering, org(最新版)
;; ├── init-core.el        ; Emacs本体の基本設定
;; ├── init-japanese.el    ; 言語環境, mozc
;; ├── init-files.el       ; recentf, auto-revert, dired, WSL連携
;; ├── init-ui.el          ; テーマ, フォント, モードライン
;; ├── init-completion.el  ; vertico, consult, corfu, cape
;; ├── init-org.el         ; org, capture, org-roam, org-modern
;; ├── init-document.el    ; AUCTeX, pandoc, orgのエクスポート
;; ├── init-editing.el     ; undo-tree, yasnippet, hydra, ウィンドウ操作
;; ├── init-vcs.el         ; magit, diff-hl
;; ├── init-programming.el ; flymake, eglot, Lisp編集, projectile
;; ├── init-ai.el          ; AI関連の設定
;; └── init-misc.el        ; 小物ツール, 未整理・要確認

;;; Code:

(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)     ; パッケージ（elpaca）の設定
(require 'init-core)        ; Emacs本体の設定
(require 'init-japanese)    ; 日本語環境
(require 'init-files)       ; ファイル操作関係
(require 'init-ui)          ; 見た目
(require 'init-completion)  ; 補完機能

(require 'init-org)         ; Orgモード用設定
(require 'init-document)    ; 文書作成・エクスポート
(require 'init-editing)     ; テキスト編集・ウィンドウ操作
(require 'init-vcs)         ; バージョン管理
(require 'init-programming) ; プログラミング全般
(require 'init-ai)          ; AI関連の設定

(require 'init-misc)        ; その他・テスト中

(message "init.el loaded")
(provide 'init)
;;; init.el ends here
