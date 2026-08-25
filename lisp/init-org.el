(use-package org
  :ensure nil
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :custom
  (org-directory "~/Dropbox/Org/")
  (org-default-notes-file "~/Dropbox/Org/Notebook.org")
  (org-agenda-files '("~/Dropbox/Org/"))
  (org-refile-targets '((org-agenda-files :tag . "REFILE")
			(nil :tag . "REFILE")))

  (org-todo-keyword-faces
   '(("NEXT" . (:foreground "blue" :underline t))
     ("DONE" . (:foreground "pale green"))))
  (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE" "SOMEDAY")))
  
  (org-startup-indented t)
  (org-startup-truncated nil)
  (org-return-follows-link  t)          ; RET/C-mでリンクを開く
  (org-agenda-start-with-follow-mode t) ; アジェンダで関連するorgファイルを開く
  ;; (org-ellipsis "↴")                   ; ▽,…,▼, ↴, ⬎, ⤷, ⋱
  (org-export-with-sub-superscripts nil) ; A^x B_z のような添字の処理をしない
  ;; (org-agenda-remove-tags t)             ; アジェンダにタグを表示しない
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  )

(use-package org-sidebar
  :bind ("C-c s" . org-sidebar-tree)
  :custom
  (org-sidebar-tree-side 'left))    
