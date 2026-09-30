;;; init-ui.el --- 見た目の設定  -*- lexical-binding: t; -*-
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

;; テーマ・フォント・フレーム・モードラインなど見た目の設定。

;;; Code:

;;; テーマの設定
(load-theme 'misterioso)

;;; フォントを設定する
;; Ubuntuの場合、~/.fontsに必要なフォントを入れて
;; # fc-cache -fv
;; を実行

;; ｜あいうえお｜
;; ｜憂鬱な檸檬｜
;; ｜<miilwiim>｜
;; ｜!"#$%&'~{}｜
;; ｜🙆iimmiim>｜
(use-package emacs
  :ensure nil
  :config
  (custom-set-faces
   '(default ((t (:family "HackGen")))) ;; (x-list-fonts "HackGen") で確認可能
   ))

;;; ウィンドウの余白と境界線
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;;; frame - 画面の最大化をトグル
(use-package frame
  :ensure nil
  :bind ("<f11>" . toggle-frame-maximized))

;;; minions - マイナーモード表示をコンパクトにする
(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]")
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))

;;; beacon - バッファ・ウィンドウ切り替え時にカーソル位置を点滅表示
(use-package beacon
  :ensure t
  :custom
  (beacon-blink-when-focused nil)
  :config
  (beacon-mode 1))

;;; hydra-zoom - 文字サイズ・行番号表示の切り替え（<f12>）
;; hydra本体の導入はinit-editing.el
(with-eval-after-load 'hydra
  (defhydra hydra-zoom (global-map "<f12>")
    "zoom"
    ("i" text-scale-increase "Zoom in")
    ("o" text-scale-decrease "Zoom out")
    ("l" global-display-line-numbers-mode "Line number")))

;; ;;; dashboard - Emacs起動時にダッシュボードを表示する
;; ;; 起動が遅い
;; (use-package dashboard
;;   :ensure t ; 必要に応じて
;;   :config
;;   ;; 1. 起動時にダッシュボードを表示
;;   (dashboard-setup-startup-hook)

;;   ;; 2. 最も重いアジェンダの読み込みを非同期化する（Emacs 29+ / dashboardの比較的新しいバージョンで有効）
;;   (setq dashboard-agenda-release-buffers t)
;;   (setq dashboard-async-services '(agenda)) ; agendaを非同期処理にする

;;   ;; 3. アイコン描画がボトルネックの場合は、明示的に無効化する
;;   (setq dashboard-set-heading-icons nil)
;;   (setq dashboard-set-file-icons nil)

;;   ;; 4. ダッシュボードに表示する項目
;;   ;; ダッシュボードに表示する項目とその件数
;;   (setq dashboard-items '((recents  . 5)   ; 最近開いたファイル
;;                           (bookmarks . 5)  ; ブックマーク
;;                           (projects . 5)   ; 最近のプロジェクト（Project.elやProjectile連携）
;;                           (agenda . 5))))   ; Org-modeのアジェンダ（予定）

(provide 'init-ui)
;;; init-ui.el ends here
