;;; init-vcs.el --- バージョン管理の設定  -*- lexical-binding: t; -*-
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

;; Git（magit・diff-hl）とEmacs標準VCの設定。

;;; Code:

;;; magit - Gitのフロントエンド
;; 内蔵されている古いtransientではなく、ELPA/MELPAの最新版を優先して読み込む
(use-package transient
  :defer t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)       ; 標準的なMagitの起動ショートカット
         ("C-x M-g" . magit-dispatch))  ; 各種Gitコマンドのポップアップ
  :config
  ;; 1. コミットメッセージ入力時に自動で折り返す（長文対策）
  (add-hook 'with-editor-mode-hook 'turn-on-auto-fill)

  ;; 2. 大規模リポジトリでの速度低下を防ぐ（Windows/Linux共通の高速化設定）
  (setq magit-refresh-status-buffer nil) ; バッファ切り替え時の自動更新を抑制

  ;; OSごとの個別最適化（Windows環境のMagitは遅くなりやすいため）
  (cond
   ((eq system-type 'windows-nt)
    ;; WindowsでMagitの挙動を高速化するハック
    (setq vc-handled-backends nil)    ; Emacs標準のVC機能を無効化してMagitに集中
    (setq magit-git-executable "git")) ; Gitのパスを明示（環境に応じてフルパスに）

   ((eq system-type 'gnu/linux)
    ;; Linux向けの特有設定があればここに記述
    nil)))

;;; diff-hl - diffをわかり易く表示
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1)
  :hook
  ((dired-mode . diff-hl-dired-mode)         ; Dired でも変更状態を表示
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)) ; Magit 操作後に即座に表示を同期
  :bind
  (("C-c v n" . diff-hl-next-hunk)           ; 次の変更箇所へジャンプ
   ("C-c v p" . diff-hl-previous-hunk)       ; 前の変更箇所へジャンプ
   ("C-c v d" . diff-hl-diff-goto-hunk)      ; 該当箇所の diff を開く
   ("C-c v r" . diff-hl-revert-hunk)         ; カーソル位置の変更を元に戻す
   ("C-c v s" . diff-hl-stage-current-hunk)) ; カーソル位置の変更のみをステージング
  :config
  ;; フリンジの見た目を少し太く・見やすく調整（お好みで）
  ;; (setq diff-hl-draw-borders nil)

  ;; フリンジがない環境（ターミナル等）や Flymake/Flycheck とフリンジが衝突する場合はマージン表示へ自動切替
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

;;; vc-hooks
(use-package vc-hooks
  :ensure nil
  :custom
  (vc-handled-backends '(Git))) ; Gitのみ使用

(provide 'init-vcs)
;;; init-vcs.el ends here
