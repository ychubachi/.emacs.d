;;; init-org.el
;;; org - 本体の設定
(use-package org
  :ensure nil                           ; 既に最新版をダウンロード済
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

  ;; TODOステートの管理
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (org-log-done 'time)                  ; タスク完了時に完了日時を自動記録

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

;;; org-reverse-datetree - キャプチャする際の日付を降順にする
;; https://github.com/akirak/org-reverse-datetree
(use-package org-reverse-datetree
  :after org)

;;; doct - org-captureの設定
(use-package doct
  :after org org-reverse-datetree
  ;;recommended: defer until calling doct
                                        ;:commands (doct)
  :config
  (setq org-capture-templates
        (doct '(("Memo" :keys "m"
                 :file "~/Dropbox/Org/Memo.org"
                 :function org-reverse-datetree-goto-date-in-file
                 :empty-lines-after 1
                 :template ("* %?"
                            ":PROPERTIES:"
                            ":CREATED: %U"
                            ":LINK: %a"
                            ":END:"))
                ("Todo" :keys "t"
                 :file "~/Dropbox/Org/Memo.org"
                 :function org-reverse-datetree-goto-date-in-file
                 :empty-lines-after 1
                 :template ("* TODO %?"
                            ":PROPERTIES:"
                            ":CREATED: %U"
                            ":LINK: %a"
                            ":END:"))
                ("Notebook" :keys "n"
                 :prepend t
                 :empty-lines-after 1
                 :file "~/Dropbox/Org/Notebook.org"
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

;;; org-tempo - #begin_...を簡単に
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

;;; org-preview-mode
;; https://github.com/jakebox/org-preview-html

(use-package org-preview-html
  :commands (org-preview-html-mode)
  :custom
  ;; プレビューの表示形式 ('eww または 'xwidget)
  (org-preview-html-viewer 'eww)
  ;; 更新タイミング ('save, 'export, 'timer, 'manual, 'instant)
  (org-preview-html-refresh-configuration 'save)
  ;; 'timer 設定時の更新間隔 (秒)
  (org-preview-html-timer-interval 2)
  :bind
  (:map org-mode-map
        ("C-c C-p" . org-preview-html-mode)))

;;; ox-latex - LaTeXエクスポート設定

;; #+TITLE: 日本語PDF出力テスト
;; #+AUTHOR: あなたの名前
;; #+LATEX_CLASS: bxjsarticle
;; #+LATEX_CLASS_OPTIONS: [a4paper,11pt]

;; 🚀 PDFの出力方法（キーバインド）
;; 1. Orgファイルを開いた状態で C-c C-e を押して、エクスポートディスパッチャーを開きます。
;; 2. l (Export to LaTeX) を選択します。
;; 3. p (As PDF file) を押してPDFを生成、または o (As PDF file and open) を押して生成後にビューアで開きます。

(use-package ox-latex
  :ensure nil
  :after org
  :custom
  ;; デフォルトのLaTeXコンパイラを lualatex に指定
  (org-latex-compiler "lualatex")
  ;; PDF生成プロセスを latexmk + lualatex に設定
  (org-latex-pdf-process
   '("latexmk -lualatex -interaction=nonstopmode -output-directory=%o %f"))
  ;; デフォルトの文書クラスを bxjsarticle に設定
  (org-latex-default-class "bxjsarticle")
  :config
  ;; 日本語向け文書クラス（bxjsarticle）の設定
  (add-to-list 'org-latex-classes
               '("bxjsarticle"
                 "\\documentclass[lualatex,ja=standard]{bxjsarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; 日本語向け文書クラス（ltjsarticle）の設定
  (add-to-list 'org-latex-classes
               '("ltjsarticle"
                 "\\documentclass[11pt]{ltjsarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; 日本語の途中で改行してもPDF出力時に余分な半角スペースが入らないようにする
  (defun my/org-latex-filter-nospace-japanese (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "\\([ぁ-んーァ-ヶー一-龠]\\)\n\\([ぁ-んーァ-ヶー一-龠]\\)" "\\1\\2" text)))
  (add-to-list 'org-export-filter-paragraph-functions 'my/org-latex-filter-nospace-japanese))

;;; TODO: クリップボードのMarkdownテキストをOrg-mode形式に変換して貼り付け

;; (defun my/paste-markdown-as-org ()
;;   "クリップボードのMarkdownテキストをOrg-mode形式に変換して貼り付けます。"
;;   (interactive)
;;   (let ((markdown-text (gui-get-selection 'CLIPBOARD 'STRING)))
;;     (if markdown-text
;;         (with-temp-buffer
;;           (insert markdown-text)
;;           (call-process-region (point-min) (point-max) "pandoc" t t nil "-f" "markdown" "-t" "org")
;;           (let ((org-text (buffer-string)))
;;             (insert-into-buffer (current-buffer) (point) (point) org-text)
;;             (kill-new org-text) ; オプション: クリップボードの中身もOrgに置き換える
;;             (yank)))
;;       (message "クリップボードが空か、テキストではありません。"))))

;; ;; 好きなキーバインドを割り当て（例: C-c C-x M-g）
;; (define-key org-mode-map (kbd "C-c C-x M-g") 'my/paste-markdown-as-org)

;;; org-modern - Org-modeの見た目を近代的に
(use-package org-modern
  :ensure t
  :custom
  (org-modern-list '((?+ . "◦") (?- . "-") (?* . "•")))
  (org-modern-star '("■" ".◆" "..●" "...＊" "....＋"))
  :config
  ;; 余白と境界線の設定
  (modify-all-frames-parameters
   '((right-divider-width . 10)
     (internal-border-width . 10)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
  (global-org-modern-mode 1))

;;; org-download - 画像のドラッグ＆ドロップ挿入
(use-package org-download
  :ensure t
  :custom
  (org-download-method 'attach)
  :config
  (setq org-image-actual-width 400)
  (add-hook 'dired-mode-hook #'org-download-enable)
  (when (eq system-type 'windows-nt)
    (setq org-download-screenshot-method "magick convert clipboard: %s")))

;;; ob-plantuml - PlantUMLによる図表生成
(use-package ob-plantuml
  :ensure nil ; org-babel 内蔵の plantuml 統合を使用
  :after org
  :config
  ;; plantuml.jarへのパスを設定
  (setq org-plantuml-jar-path (expand-file-name "lib/plantuml-1.2022.12.jar" user-emacs-directory))
  ;; org-babelで使用する言語を登録
  (add-to-list 'org-babel-load-languages '(plantuml . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;;; ---------------------------------------------------------
;;; 移さなくて良いもの（コメントアウト）
;;; ---------------------------------------------------------
;; ;; 見出し位置での1キーナビゲーション（慣れないと意図しない誤動作を起こしやすいため不要）
;; (setq org-use-speed-commands
;;       (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))

;; ;; クリップボードのMarkdownを自動でOrg形式に変換して貼り付け（外部コマンド `pandoc` に依存するため不要）
;; (defun my/paste-markdown-as-org ()
;;   "クリップボードのMarkdownテキストをOrg-mode形式に変換して貼り付けます。"
;;   (interactive)
;;   (let ((markdown-text (gui-get-selection 'CLIPBOARD 'STRING)))
;;     (if markdown-text
;;         (with-temp-buffer
;;           (insert markdown-text)
;;           (call-process-region (point-min) (point-max) "pandoc" t t nil "-f" "markdown" "-t" "org")
;;           (let ((org-text (buffer-string)))
;;             (insert-into-buffer (current-buffer) (point) (point) org-text)
;;             (kill-new org-text)
;;             (yank)))
;;       (message "クリップボードが空か、テキストではありません。"))))
;; (define-key org-mode-map (kbd "C-c C-x M-g") 'my/paste-markdown-as-org)

;;; フッター
(provide 'init-org)
