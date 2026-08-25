;;; recentf
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)

  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup 'never)

  :config
  (run-at-time nil (* 5 60)
               #'recentf-save-list))

;;; savehist
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;;; saveplace
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))

;;; auto-revert
(use-package autorevert
  :ensure nil

  :custom
  (auto-revert-interval 1)
  (auto-revert-verbose nil)

  :init
  (global-auto-revert-mode 1))

;;; files
(use-package files
  :ensure nil

  :custom
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)

  ;; シンボリックリンクを自動で辿る
  (vc-follow-symlinks t))

;;; dired
(use-package dired
  :ensure nil

  :custom
  (dired-dwim-target t))

;;; wdired
(use-package wdired
  :ensure nil

  :bind
  (:map dired-mode-map
        ("r" . wdired-change-to-wdired-mode)))

(provide 'init-files)
