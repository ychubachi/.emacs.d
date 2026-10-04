;;; init-japanese.el --- 日本語環境の設定  -*- lexical-binding: t; -*-
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

;; 言語環境・文字コードと日本語入力（mozc）の設定。

;;; Code:

;;; 言語環境と文字コードを設定する
(use-package emacs
  :ensure nil
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (cond ((eq system-type 'windows-nt)
	 (setq default-process-coding-system
	       (cons 'utf-8 'cp932-unix)))))

;;; mozc - 日本語変換用ヘルパーの呼び出し設定
;; WSL(Ubuntu) から利用する場合:
;; - mozc_emacs_helper.sh を作成し、Windows側のexeを呼び出す
(use-package mozc
  :demand t
  :config
  (cond
   ((eq system-type 'windows-nt)
    (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper-2.31.exe"))
   (t
    ;; helperのVer 2.31
    (setq mozc-helper-program-name "mozc_emacs_helper.sh"))))

;;; mozc-im - インプット方式の設定
(use-package mozc-im
  :after mozc
  :demand t
  :bind
  (("C-o" . toggle-input-method))
  :init
  (setq default-input-method "japanese-mozc-im")

  ;; モードによってカーソルの色を変える
  (defvar my/cursor-color-japanese "cyan")
  (defvar my/cursor-color-default (frame-parameter nil 'cursor-color))
  (add-hook 'input-method-activate-hook
            (lambda () (set-cursor-color my/cursor-color-japanese)))
  (add-hook 'input-method-deactivate-hook
            (lambda () (set-cursor-color my/cursor-color-default)))
  )

(provide 'init-japanese)
;;; init-japanese.el ends here
