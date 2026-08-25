;;; テーマの設定
(load-theme 'misterioso)

;;; dashboard - Emacs起動時にダッシュボードを表示する

(use-package dashboard
  :config
  ;; スタートアップフックに登録して起動時に表示
  (dashboard-setup-startup-hook)

  ;; ダッシュボードに表示する項目とその件数
  (setq dashboard-items '((recents  . 5)   ; 最近開いたファイル
                          (bookmarks . 5)  ; ブックマーク
                          (projects . 5)   ; 最近のプロジェクト（Project.elやProjectile連携）
                          (agenda . 5))))   ; Org-modeのアジェンダ（予定）
(provide 'init-ui)
