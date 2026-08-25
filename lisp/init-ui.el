;;; テーマの設定
(load-theme 'misterioso)

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
