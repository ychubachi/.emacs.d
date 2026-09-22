;;;; init-misc.el --- 未整理・要確認の設定置き場
;;
;; init.old.org（旧設定）からの移植候補のうち、現行のlisp/*.elに
;; 既に取り込んだもの・明らかに不要と判断したものは削除済み。
;; ここに残っているのは (1) 使うかどうか判断がつかない設定、
;; (2) 本人に利用状況を確認してから移植/削除を決めたい設定。

;;; 未検討（保留）

;; (use-package display-fill-column-indicator
;;   :hook
;;   (emacs-startup-hook . global-display-fill-column-indicator-mode))

;; (use-package midnight
;;   :url "https://www.emacswiki.org/emacs/MidnightMode"
;;   :custom
;;   ((clean-buffer-list-delay-general . 1))
;;   :hook
;;   (emacs-startup-hook . midnight-mode))

;; (use-package whitespace
;;   :init
;;   (setq whitespace-style
;;         '(
;;           face                  ; faceで可視化
;;           trailing              ; 行末
;;           tabs                  ; タブ
;;           spaces                ; スペース
;;           space-mark            ; 表示のマッピング
;;           tab-mark
;;           ))
;;   (setq whitespace-display-mappings
;;         '(
;;           (space-mark ?\u3000 [?□])
;;           (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
;;           ))
;;   (setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
;;   (setq whitespace-space-regexp "\\(\u3000+\\)")
;;   (global-whitespace-mode t))

;; (use-package imenu-list
;;   :bind (("C-c i" . imenu-list-smart-toggle))
;;   :hook
;;   (imenu-list-major-mode-hook . (lambda nil (display-line-numbers-mode -1))))

;; (add-hook 'org-mode-hook
;;           (lambda () (imenu-add-to-menubar "Imenu")))
;; (setq org-imenu-depth 3)
;; (add-hook 'org-mode-hook 'imenu-list-minor-mode)

;; (use-package moody
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

;; (use-package ruler-mode
;;   :config
;;   (add-hook 'find-file-hook (lambda () (ruler-mode 1))))

;;; 要確認 - 本人に利用状況を確認してから移植/削除を決める

;; org-publish-project-alist (chubachi.net向け) - SCP経由のpublishを今も使うか
;; init-org.elのmy/org-publish-current-siteは汎用の別方式
;; (use-package org-publish-project-alist
;;   :config
;;   (setq org-publish-project-alist
;;         '(("chubachi.net"
;;            :components ("chubachi.net-orgfiles" "chubachi.net-others"))
;;           ("chubachi.net-orgfiles"
;;            :publishing-function org-html-publish-to-html
;;            :base-directory "~/Dropbox/Org/publish/chubachi.net/"
;;            :publishing-directory "/scpx:chubachi@chubachi.sakura.ne.jp:~/www/chubachi.net/publish"
;;            :base-extension "org"
;;            :recursive t)
;;           ("chubachi.net-others"
;;            :publishing-function org-publish-attachment
;;            :base-directory "~/Dropbox/Org/publish/chubachi.net/"
;;            :publishing-directory "/scpx:chubachi@chubachi.sakura.ne.jp:~/www/chubachi.net/publish/"
;;            :base-extension "jpg\\|gif\\|png|css\\|el"
;;            :recursive t))))

;;; free-keys - 空いているキーバインドを確認する
(use-package free-keys
  :ensure t
  :commands free-keys)

(provide 'init-misc)
