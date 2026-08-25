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
;; ├── init-core.el ; package, use-package, encoding
;; ├── init-ui.el ; theme, font, modeline
;; ├── init-editing.el ; whitespace, electric-pair
;; ├── init-completion.el ; vertico, orderless, corfu
;; ├── init-vcs.el ; magit, diff-hl
;; ├── init-org.el ; org-mode
;; ├── init-evil.el
;; ├── init-mozc.el
;; └── init-programming.el ; lsp, treesit, eglot

;;; Code:

(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

(require 'init-package); パッケージ（elpaca）の設定
(require 'init-core) ; Orgの初期化、Emacs本体の設定
(require 'init-japanese) ; mozcの設定
(require 'init-files) ; ファイル操作関係
(require 'init-ui) ; テーマの設定（のみ）
(require 'init-completion) ; 補完機能
(require 'init-editing)
(require 'init-programming)
(require 'init-org)
(require 'init-misc)

(message "init.el loaded")
(provide 'init)
;;; init.el ends here
