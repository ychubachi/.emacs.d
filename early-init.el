;;; early-init.el --- My early-init.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2025 Yoshihide Chubachi

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

;; このファイルはearly-init.orgから手動で生成します（C-c C-v C-t）。

;;; Code:

;;; 起動時にガベージコレクションのしきい値を大きくして高速化
(setq gc-cons-threshold 100000000)

(setq package-enable-at-startup nil)
;;; 起動時にフルスクリーンしアイコン（タスクバー）を消す
;; (add-to-list 'default-frame-alist '(fullscreen . fullboth))
;; (tool-bar-mode -1)
(cond
 ;; --------------------------------------------------
 ;; 1. Windows の場合
 ;; --------------------------------------------------
 ((eq system-type 'windows-nt)
  (menu-bar-mode 1)                                ; メニューバーを表示
  (add-to-list 'default-frame-alist '(undecorated . nil)) ; タイトルバーを表示
  (add-to-list 'default-frame-alist '(fullscreen . maximized))) ; 画面最大化

 ;; --------------------------------------------------
 ;; 2. Linux の場合
 ;; --------------------------------------------------
 ((eq system-type 'gnu/linux)
  (menu-bar-mode 1)                                ; メニューバーを表示
  (add-to-list 'default-frame-alist '(undecorated . nil)) ; タイトルバーを表示
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))) ; 画面最大化

;;; early-init.el ends here
