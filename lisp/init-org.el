;;; init-org.el
;;; org - 本体の設定
(use-package org
  :ensure nil                           ; 既にダウンロード済
  :bind
  (("C-c a" . org-agenda)               ; アジェンダビューを開く
   ("C-c c" . org-capture)              ; クイックメモ・タスク記録
   ("C-c l" . org-store-link))          ; 現在のバッファ位置へのリンクを保存
  :custom
  ;; 基本ディレクトリ・アジェンダ対象ファイルの設定
  (org-directory "~/Dropbox/Org/")
  (org-default-notes-file "~/Dropbox/Org/Notebook.org")
  (org-agenda-files '("~/Dropbox/Org/"))

  ;; 見た目・編集の快適化
  (org-startup-indented t)              ; 見出しの階層に合わせて自動インデント
  (org-startup-folded 'content)         ; ファイルを開いた時は見出しのみ表示
  (org-hide-leading-stars t)   ; 見出しの余分な '*' を非表示にしてスッキリ見せる
  (org-use-sub-superscripts '{}) ; '_' で誤って下付き文字になるのを防ぐ (波括弧のみ許可)
  (org-return-follows-link t)    ; リンク上で Enter を押すとリンク先へジャンプ
  (org-refile-targets '((org-agenda-files :tag . "REFILE")
			(nil :tag . "REFILE")))
  (org-startup-truncated nil)
  (org-agenda-start-with-follow-mode t)  ; アジェンダで関連するorgファイルを開く
  (org-export-with-sub-superscripts nil) ; A^x B_z のような添字の処理をしない
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  ;; (org-ellipsis "↴")                   ; ▽,…,▼, ↴, ⬎, ⤷, ⋱
  ;; (org-agenda-remove-tags t)             ; アジェンダにタグを表示しない

  ;; TODO ステートの管理
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (org-log-done 'time)                  ; タスク完了時に完了日時を自動記録

  ;; ;; 4. Org Capture テンプレート (C-c c で即座にメモ・タスク作成)
  ;; (org-capture-templates
  ;;  '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
  ;;     "* TODO %?\n  作成日時: %U\n  %a\n  %i" :empty-lines 1)
  ;;    ("m" "Quick Memo" entry (file+headline "~/org/inbox.org" "Memos")
  ;;     "* %?\n  記録日時: %U\n  %i" :empty-lines 1)))

  ;; ソースコードブロック (Org Babel) の設定
  (org-src-fontify-natively t) ; コードブロック内を各メジャーモードの色でハイライト
  (org-src-tab-acts-natively t) ; コードブロック内の Tab 動作を言語モードに合わせる
  (org-edit-src-content-indentation 0) ; コードブロック編集時の余分なインデントを防止
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     (python . t)
     (shell . t))))

;;; doct - org-captureの設定
  (use-package doct
          :straight t
          ;;recommended: defer until calling doct
                                          ;:commands (doct)
          :config
          (setq org-capture-templates
                (doct '(("Memo" :keys "m"
                         :file "~/Dropbox/Org/Memo.org"
                         ; :empty-lines-before 1
                         :empty-lines-after 1
                         :datetree t
                         :unnarrowed nil       ; t
                         :jump-to-captured nil ;
                         :template ("* %?"
                                    ":PROPERTIES:"
                                    ":CREATED: %U"
                                    ":LINK: %a"
                                    ":END:"))
                        ("Memo (unarrowed)" :keys "M"
                         :file "~/Dropbox/Org/Memo.org"
                         ; :empty-lines-before 1
                         :empty-lines-after 1
                         :datetree t
                         :unnarrowed t
                         :jump-to-captured t
                         :template ("* %?"
                                    ":PROPERTIES:"
                                    ":CREATED: %U"
                                    ":LINK: %a"
                                    ":END:"))
                        ("Todo" :keys "t"
                         :file "~/Dropbox/Org/Memo.org"
                         :datetree t
                         :empty-lines-before 1
                         :template ("* TODO %?"
                                    ":PROPERTIES:"
                                    ":CREATED: %U"
                                    ":LINK: %a"
                                    ":END:"))
                        ("Notebook" :keys "n"
                         :prepend t
                         :empty-lines-after 1
                         :file "~/Dropbox/Org/Notebook.org"
                         :unnarrowed t
                         :template ("* %^{Description}"
                                    ":PROPERTIES:"
                                    ":CREATED: %T"
                                    ":END:"
                                    "\n%?"))
                        ("Post" :keys "p"
                         :file "~/Dropbox/Org/Memo.org"
                         :datetree t
                         :unnarrowed t
                         :jump-to-captured nil
                         :empty-lines-before 1
                                          ; :empty-lines-after 1
                         :todo-state "TODO"
                         :export_file_name (lambda () (concat (format-time-string "%Y-%m-%d-%H-%M-%S") ".html"))
                         :template ("* %{todo-state} %^{Headline} :POST:"
                                    ":PROPERTIES:"
                                    ":CREATED: %U"
                                    ":EXPORT_FILE_NAME: ~/git/ploversky-jekyll/_drafts/drafts_%{export_file_name}"
                                    ":EXPORT_OPTIONS: toc:nil num:nil html5-fancy:t"
                                    ":EXPORT_HTML_DOCTYPE: html5"
                                    ":DIR: ~/git/ploversky-jekyll/assets/images/posts/"
                                    ":END:"
                                    ""
                                    "#+begin_comment"
                                    "First time: C-c C-e C-b C-s h h (Do this here)"
                                    "Next  time: C-u C-c C-e         (Do this anyware in the subtree)"
                                    "#+end_comment"
                                    ""
                                    "#+begin_export html"
                                    "---"
                                    "layout: post"
                                    "title:"
                                    "categories:"
                                    "tags:"
                                    "published: true"
                                    "---"
                                    "#+end_export"
                                    "\n**  %?"))
                        ("Blog" :keys "b"
                         :prepend t
                         :empty-lines-after 1
                         :unnarrowed t
                         :children
                         (("ploversky@zenn.dev" :keys "z"
                           :file "~/git/ploversky-zenn.dev/plaversky@zenn.dev.org"
                           :headline   "記事"
                           :todo-state "TODO"
                           :export_file_name (lambda () (concat (format-time-string "%Y%m%d-%H%M%S")))
                           :template ("* %{todo-state} %^{Description}"
                                      ":PROPERTIES:"
                                      ":CREATED: %T"
                                      ":EXPORT_FILE_NAME: articles/%{export_file_name}"
                                      ":EXPORT_GFM_TAGS: blog"
                                      ":EXPORT_GFM_CUSTOM_FRONT_MATTER: :emoji 👩‍💻"
                                      ":EXPORT_GFM_CUSTOM_FRONT_MATTER+: :type tech"
                                      ":EXPORT_GFM_CUSTOM_FRONT_MATTER+: :published false"
                                      ":END:"
                                      "\n* %?"))
                          ("blog.chubachi.net"  :keys "b"
                           :file "~/git/ychubachi.github.io/blog.chubachi.net.org"
                           :headline   "Blog"
                           :todo-state "TODO"
                           :export_file_name (lambda () (concat (format-time-string "%Y%m%d-%H%M%S")))
                           :template ("* %{todo-state} %^{Description}"
                                      ":PROPERTIES:"
                                      ":CREATED: %T"
                                      ":EXPORT_FILE_NAME: %{export_file_name}"
                                      ":EXPORT_DATE: %U"
                                      ":END:"
                                      "\n** %?"))))))))

;;; org-sidebar - Orgの構造をサイドバーに表示
(use-package org-sidebar
  :bind ("C-c t" . org-sidebar-tree)
  :custom
  (org-sidebar-tree-side 'left))

;;; org-tempo - begin_...を簡単に
;; <el TAB -> #begin_src elisp

(use-package org-tempo
  :ensure nil ; 内蔵パッケージ
  :config
  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist
               '("sh" . "src bash"))
  (add-to-list 'org-structure-template-alist
               '("rb" . "src ruby :results output"))
  (add-to-list 'org-structure-template-alist
               '("j"  . "src java :results output"))
  (add-to-list 'org-structure-template-alist
               '("py" . "src python :results output"))
  (add-to-list 'org-structure-template-alist
               '("n" . "note"))
  (add-to-list 'org-structure-template-alist
               '("w" . "warning"))
  (add-to-list 'org-structure-template-alist
               '("f" . "figure"))
  (add-to-list 'org-structure-template-alist
               '("ai" . "ai")))

(provide 'init-org)
