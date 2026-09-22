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

;; nerd-icons - all-the-iconsより多くのアイコン。旧ファイルには「インストールできない」との記録あり
;; (use-package nerd-icons )

;; pandoc-mode - Pandoc経由の文書変換を今も使うか
;; (use-package pandoc-mode
;;   :after hydra)

;; メールクライアント一式 - 今もEmacsでメールを読み書きしているか
;; (use-package notmuch
;;       :when (not (eq system-type 'windows-nt))
;;       :require t
;;       :hook
;;       (notmuch-message-mode-hook . visual-fill-column-mode)
;;       (notmuch-message-mode-hook . (lambda () (auto-fill-mode -1)))
;;       :custom
;;       ((notmuch-draft-folder . "/drafts") ; 編集中のドラフトはローカルのフォルダに
;;        (notmuch-fcc-dirs . nil)           ; 送信済みメールはローカルに保存せず
;;                                       ; Gmailに任せる
;;        (notmuch-search-oldest-first . nil) ; 検索結果を新しい順でソート
;;        (notmuch-saved-searches
;;         . '((:name "flagged"    :query "tag:flagged AND NOT tag:deleted"
;;                    :key "f" :search-type tree)
;;             (:name "inbox"      :query "tag:inbox folder:/Gmail\\/inbox/ AND NOT tag:deleted"
;;                    :key "i" :search-type tree)
;;             (:name "unread"     :query "tag:unread AND NOT tag:deleted"
;;                    :key "u" :search-type tree)
;;             (:name "sent"       :query "tag:sent AND NOT tag:deleted"
;;                    :key "s" :search-type tree)
;;             (:name "drafts"     :query "tag:draft AND NOT tag:deleted"
;;                    :key "d" :search-type tree)
;;             (:name "Gmal Inbox" :query "folder:/Gmail\\/inbox/"
;;                    :key "I" :search-type tree)
;;             (:name "Gmal Sent"  :query "folder:/Gmail\\/sent/"
;;                    :key "S" :search-type tree)
;;             (:name "all mail"   :query "NOT tag:deleted"
;;                    :key "a" :search-type tree)))
;;        )
;;       :bind (("C-c r" . notmuch-hello))
;;       :config
;;       (advice-add #'notmuch-read-tag-changes
;;                   :filter-return (lambda (x) (mapcar #'string-trim x))) ; vertico対策
;;       :config
;;       (define-key notmuch-search-mode-map "f"
;;         (lambda ()
;;           "toggle flaged tag for message"
;;           (interactive)
;;           (if (member "flagged" (notmuch-search-get-tags))
;;               (notmuch-search-tag (list "-flagged"))
;;             (notmuch-search-tag (list "+flagged")))))
;;       (define-key notmuch-show-mode-map "f"
;;         (lambda ()
;;           "toggle flaged tag for message"
;;           (interactive)
;;           (if (member "flagged" (notmuch-show-get-tags))
;;               (notmuch-show-tag (list "-flagged"))
;;             (notmuch-show-tag (list "+flagged")))))
;;       (define-key notmuch-tree-mode-map "f"
;;         (lambda ()
;;           "toggle flaged tag for message"
;;           (interactive)
;;           (if (member "flagged" (notmuch-tree-get-tags))
;;               (notmuch-tree-tag (list "-flagged"))
;;             (notmuch-tree-tag (list "+flagged"))))))

;; (use-package ol-notmuch
;;   :require t
;;   :after notmuch org)

;; (use-package consult-notmuch
;;   :when (not (eq system-type 'windows-nt))
;;   :after consult notmuch)

;; (use-package mm-decode
;;   :custom (mm-default-directory . "~/Downloads/"))

;; (use-package gnus-alias
;;   :straight (gnus-alias :type git :host github
;;                         :repo "hexmode/gnus-alias")
;;   :config
;;   (setq gnus-alias-identity-alist
;;         '(("work"
;;            nil
;;            "中鉢欣秀 <yc@aiit.ac.jp>"
;;            nil            ;; No organization header
;;            nil            ;; No extra headers
;;            nil            ;; No extra body text
;;            "~/.signature" ;; My signature
;;            ))))

;; (use-package wanderlust
;;   :config
;;   ;; IMAP
;;   (setq elmo-imap4-default-user "yc@aiit.ac.jp"
;;         elmo-imap4-default-authenticate-type 'clear
;;         elmo-imap4-default-server "imap.gmail.com"
;;         elmo-imap4-default-port 993
;;         elmo-imap4-default-stream-type 'ssl
;;         )
;;   (setq elmo-imap4-use-modified-utf7 t)
;;   (setq elmo-message-fetch-threshold nil)
;;   (setq
;;    wl-smtp-connection-type   'starttls        ; Use TLS
;;    wl-smtp-authenticate-type "login"          ; Authentication type
;;    wl-smtp-posting-user      "yc@aiit.ac.jp"  ; Username
;;    wl-smtp-posting-server    "smtp.gmail.com" ; SMTP server
;;    wl-smtp-posting-port      587              ; The SMTP port
;;    wl-local-domain           "aiit.ac.jp"  ; The SMTP server again
;;    wl-message-id-domain      "aiit.ac.jp") ; And... Again?
;;   (setq
;;    wl-default-folder "%INBOX"
;;    wl-draft-folder   "%[Gmail]/下書き"
;;    wl-trash-folder   "%[Gmail]/ゴミ箱"
;;    wl-from "Yoshihide Chubachi <yc@aiit.ac.jp>" ; Our From: header field
;;    wl-fcc-force-as-read t  ; Mark sent mail (in the wl-fcc folder) as read
;;    wl-default-spec "%")    ; For auto-completion
;;   (setq wl-message-ignored-field-list
;;         '("ARC-.*:" "X-.*:" ".*Received.*:"
;;           "Authentication-Results:" "MIME-Version:"
;;           "List-.*:" "DKIM-.*:"
;;           ".*Path:" ".*Id:" "^References:"
;;           "^Replied:" "^Errors-To:"
;;           "^Lines:" "^Sender:" ".*Host:" "^Xref:"
;;           "^Content-Type:" "^Precedence:"
;;           "^Status:" "^X-VM-.*:"))
;;   (setq wl-message-visible-field-list '("^Message-Id:"))
;;   (setq mime-edit-split-message nil)
;;   (require 'wl-qs)
;;   (setq wl-quicksearch-folder "%[Gmail]/すべてのメール")
;;   (add-to-list 'wl-dispose-folder-alist
;;                '("^%INBOX" . remove))
;;   (add-to-list 'wl-dispose-folder-alist
;;                '(".*Junk$" . remove))
;;   (require 'elmo nil 'noerror)
;;   (defun my:wl-summary-jump-to-referer-message ()
;;     (interactive)
;;     (when (wl-summary-message-number)
;;       (if (eq (elmo-folder-type-internal wl-summary-buffer-elmo-folder) 'flag)
;;           (progn
;;             (let* ((referer (elmo-flag-folder-referrer
;;                              wl-summary-buffer-elmo-folder
;;                              (wl-summary-message-number)))
;;                    (folder (if (> (length referer) 1)
;;                                (completing-read
;;                                 (format "Jump to (%s): " (car (car referer)))
;;                                 referer
;;                                 nil t nil nil (car (car referer)))
;;                              (car (car referer)))))
;;               (wl-summary-goto-folder-subr folder 'no-sync nil nil t)
;;               (wl-summary-jump-to-msg (cdr (assoc folder referer)))))
;;         (when (eq (elmo-folder-type wl-summary-last-visited-folder) 'internal)
;;           (wl-summary-goto-last-visited-folder)))))
;;   (define-key wl-summary-mode-map "=" 'my:wl-summary-jump-to-referer-message))

;; ox-zenn - Zennへの投稿を今も行うか（init-org.elのdoctキャプチャにはテンプレートが残存）
;; (use-package ox-zenn
;;   :after org
;;   :require t ox-publish
;;   :defun zenn/f-parent org-publish
;;   :defvar org-publish-project-alist
;;   :preface
;;   (defvar zenn/org-dir "~/git/zenn-content")
;;   (defun zenn/org-publish (arg)
;;     "Publish zenn blog files."
;;     (interactive "P")
;;     (let ((force (or (equal '(4) arg) (equal '(64) arg)))
;;           (async (or (equal '(16) arg) (equal '(64) arg))))
;;       (org-publish "zenn" arg force async)))
;;   :config
;;   (setf
;;    (alist-get "zenn" org-publish-project-alist nil nil #'string=)
;;    (list
;;     :base-directory (expand-file-name "" zenn/org-dir)
;;     :base-extension "org"
;;     :publishing-directory (expand-file-name "../" zenn/org-dir)
;;     :recursive t
;;     :publishing-function 'org-zenn-publish-to-markdown)))

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

;; org-sync-gtasks / org-sync-qiita (自作パッケージ) - Google Tasks/Qiita同期を今も使うか
;; (use-package org-sync-gtasks
;;   :init
;;   (use-package oauth2 )
;;   :config
;;   (setq load-path (cons "~/git/org-sync-gtasks" load-path))
;;   (require 'org-sync-gtasks))

;; (use-package org-sync-qiita
;;   :init
;;   (use-package request-deferred )
;;   (use-package ox-qmd )
;;   :config
;;   (setq load-path (cons "~/git/org-sync-qiita" load-path))
;;   (require 'org-sync-qiita))

;; org-roam / org-roam-ui - 個人知識ベースを今も使うか
;; (use-package org-roam
;;   :init
;;   (setq browse-url-galeon-program nil)
;;   (setq browse-url-netscape-program nil)
;;   :custom
;;   (org-roam-directory . "~/Dropbox/Org/Roam")
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph)
;;          ("C-c n i" . org-roam-node-insert)
;;          ("C-c n c" . org-roam-capture)
;;          ("C-c n j" . org-roam-dailies-capture-today))
;;   :config
;;   (org-roam-db-autosync-mode)
;;   (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
;;   (require 'org-roam-protocol))

;; (use-package org-roam-ui
;;   :after org-roam
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

;;; free-keys - 空いているキーバインドを確認する
(use-package free-keys
  :ensure t
  :commands free-keys)

(provide 'init-misc)
