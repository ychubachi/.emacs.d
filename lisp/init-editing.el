;;; init-editing.el --- テキスト編集・ウィンドウ操作の設定  -*- lexical-binding: t; -*-
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

;; テキスト編集全般と、ウィンドウ・バッファ操作の設定。

;;; Code:

;;; undo-tree - C-zでUndoツリーを表示する
(use-package undo-tree
  :demand t
  :bind ("C-z" . undo-tree-visualize)
  :config
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;;; outli - Orgぽく使えるアウトラインモード
;; https://github.com/jdtsmith/outli

(use-package outli
  ;:after lispy ; uncomment only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading)))
              ("C-c C-n" . outline-next-visible-heading))
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer

;;; multiple-cursors - 複数カーソル同時編集
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

;;; yasnippet - テンプレート挿入機能
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "etc/yasnippet/snippets" user-emacs-directory)))
  :hook
  (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; hydra - 複数キーの連続操作をまとめる
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-buffer-menu (:color pink :hint nil)
    "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
    ("m" Buffer-menu-mark)
    ("u" Buffer-menu-unmark)
    ("U" Buffer-menu-backup-unmark)
    ("d" Buffer-menu-delete)
    ("D" Buffer-menu-delete-backwards)
    ("s" Buffer-menu-save)
    ("~" Buffer-menu-not-modified)
    ("x" Buffer-menu-execute)
    ("b" Buffer-menu-bury)
    ("g" revert-buffer)
    ("T" Buffer-menu-toggle-files-only)
    ("O" Buffer-menu-multi-occur :color blue)
    ("I" Buffer-menu-isearch-buffers :color blue)
    ("R" Buffer-menu-isearch-buffers-regexp :color blue)
    ("c" nil "cancel")
    ("v" Buffer-menu-select "select" :color blue)
    ("o" Buffer-menu-other-window "other-window" :color blue)
    ("q" quit-window "quit" :color blue))
  (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body))

;;; ウィンドウ・バッファ操作

;;;; ace-window - ウィンドウにラベルを表示して素早く移動・操作する
(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)) ; Dvorak配列のホームポジション（QWERTYのa s d f g h j k lと同じ物理キー）
  (aw-scope 'frame)
  (aw-background t)
  :custom-face
  (aw-leading-char-face ((t (:height 3.0 :foreground "red")))))

;;;; swap-buffers - 隣のウィンドウとバッファを入れ替え
(use-package swap-buffers
  :ensure t
  :bind
  ("C-c b" . swap-buffers)
  :custom
  ;; Dvorak配列のホームポジション（ace-windowと同じ考え方）
  (swap-buffers-qwerty-shortcuts '("a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-")))

;;;; perspective - バッファをグループ化して切り替える
(use-package perspective
  :ensure t
  :bind
  (("C-x C-b" . persp-list-buffers))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (persp-mode 1))

(provide 'init-editing)
;;; init-editing.el ends here
