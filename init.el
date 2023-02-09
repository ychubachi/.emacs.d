;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Yoshihide Chubachi

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

;;  My init.el.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Start profiling
(progn
  (defvar my-tick-previous-time (current-time))

  (defun my-tick-init-time (msg)
    "Tick boot sequence."
    (let ((ctime (current-time)))
      (message "--- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my-tick-previous-time)))
               msg)
      (setq my-tick-previous-time ctime)))

  (my-tick-init-time "start") ; Start profiling
  )

;;;; Package management tools

;; See: https://github.com/conao3/leaf.el#install
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))

(leaf *Leaf-Related-Packages
  :config
  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))

  (leaf leaf-tree :ensure t
    ;; :custom ((imenu-list-size . 30)
    ;;          (imenu-list-position . 'left))
    )

  (leaf leaf-convert :ensure t)

  (leaf *straight.el
  :init
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))))

(leaf *Hi-Priority-Packages
  :init
  (leaf no-littering
    :url "https://github.com/emacscollective/no-littering#usage"
    :straight t :require t)

  (leaf org :straight t)

  (leaf macrostep ; to test leaf macros.
    :doc "interactive macro expander"
    :url "https://github.com/joddie/macrostep"
    :straight t
    :bind (("C-c e" . macrostep-expand))))

(leaf *Keyboard-Sttings
  :init
  (leaf *Help-Keys
    :init
    (define-key key-translation-map [?\C-h] [?\C-?])
    (global-set-key (kbd "C-^") help-map))

  (leaf *undo :bind (("C-z" . undo))))

(leaf *Japanese-Related-Settings
  :init
  (leaf *Language-Environment
    :init
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8)
    (cond ((eq system-type 'windows-nt)
           (setq default-process-coding-system (cons 'utf-8 'cp932-unix)))))

  (leaf *Windows-Fonts
    :disabled t
    :if (eq system-type 'windows-nt)
    :init
    ;; 通常使用するフォント
    (set-frame-font "PlemolJP-12" nil t)
    ;; IME未確定時のフォント設定
    (modify-all-frames-parameters '((ime-font . "PlemolJP-12"))))

  (leaf mozc
  :straight t
  :config
  (cond ((eq system-type 'windows-nt)
         (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper.exe"))
        (t (setq mozc-helper-program-name "mozc_emacs_helper")))
  ;; (if (getenv "WSLENV")
  ;;     ;; (setq mozc-helper-program-name "mozc_emacs_helper_win.sh")
  ;;     (setq mozc-helper-program-name "mozc_emacs_helper")
  ;;   (setq mozc-helper-program-name "mozc_emacs_helper"))

  (leaf mozc-im
    :straight t
    :require t
    :custom ((default-input-method . "japanese-mozc-im"))
    :bind* (("C-o" . toggle-input-method))
    :config
    (setq mozc-candidate-style 'echo-area))

  (leaf mozc-cursor-color
    :straight (mozc-cursor-color :type git :host github
                                 :repo "iRi-E/mozc-el-extensions")
    :require t
    :config
    (setq mozc-cursor-color-alist
          '((direct        . "gray")
            (read-only     . "yellow")
            (hiragana      . "green")
            (full-katakana . "goldenrod")
            (half-ascii    . "dark orchid")
            (full-ascii    . "orchid")
            (half-katakana . "dark goldenrod")))
    ;; mozc-cursor-color を利用するための対策（NTEmacs@ウィキより）
    (defvar-local mozc-im-mode nil)
    (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
    (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
    (advice-add 'mozc-cursor-color-update
                :around (lambda (orig-fun &rest args)
                          (let ((mozc-mode mozc-im-mode))
                            (apply orig-fun args)))))
  (leaf *mozc-win
    :if (eq system-type 'windows-nt)
    :config
    (advice-add 'mozc-session-execute-command
  	        :after (lambda (&rest args)
  	                 (when (eq (nth 0 args) 'CreateSession)
  	                   (mozc-session-sendkey '(Hankaku/Zenkaku)))))))

  )

(leaf *Look-And-Feel
  :custom
  (line-spacing . 0.2)
  :init
  (leaf modus-themes
    :straight t                        ; omit this to use the built-in themes
    :custom
    (modus-themes-italic-constructs . t)
    (modus-themes-bold-constructs . nil)
    (modus-themes-region . '(bg-only no-extend))
    (modus-themes-org-blocks . 'gray-background) ; {nil,'gray-background,'tinted-background}
    (modus-themes-subtle-line-numbers . t)
    (modus-themes-mode-line . '(moody borderless (padding . 0) (height . 0.9)))
    (modus-themes-syntax . '(yellow-comments green-strings))
    ;; (modus-themes-hl-line . '(underline accented)) ;'(underline accented)
    (modus-themes-paren-match . '(intense underline))
    (modus-themes-headings ; this is an alist: read the manual or its doc string
     . '((1 . (regular 1.215))
         (2 . (regular 1.138))
         (3 . (regular 1.067))
         (t . (regular))))
    :init
    (require-theme 'modus-themes)
    ;; Load the theme of your choice:
    ;; (load-theme 'modus-operandi :no-confirm)
    (load-theme 'modus-vivendi :no-confirm)
    :bind
    ("<f5>" . modus-themes-toggle))

    (leaf org-modern
    :disabled nil
    :url "https://github.com/minad/org-modern"
    :straight t
    :init
    ;; Add frame borders and window dividers
    (modify-all-frames-parameters
     '((right-divider-width . 10)
       (internal-border-width . 10)))
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
      (face-spec-reset-face face)
      (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background))

    ;; (setq org-modern-star '("🟩" "🟣" "🔶" "◎" "○" "※"))
    ;; (setq org-modern-star '("■" "◆" "◎" "○" "§" "¶"))
    ;; (setq org-modern-star '("🟧" "🔶" "🟠" "🔸" "§" "¶"))
    (setq org-modern-star '("■" "◆" "●" "◎" "○" "・"))

    (setq
     ;; ;; Edit settings
     org-auto-align-tags nil ; Non-nil keeps tags aligned when modifying headlines.
     ;; org-tags-column 0
     ;; org-catch-invisible-edits 'show-and-error
     ;; org-special-ctrl-a/e t

     ;; ;; Org styling, hide markup etc.
     org-hide-emphasis-markers t
     ;; org-pretty-entities t
     ;; org-ellipsis "…"

     ;; ;; Agenda styling
     ;; org-agenda-tags-column 0
     ;; org-agenda-block-separator ?─
     ;; org-agenda-time-grid
     ;; '((daily today require-timed)
     ;;   (800 1000 1200 1400 1600 1800 2000)
     ;;   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
     ;; org-agenda-current-time-string
     ;; "⭠ now ─────────────────────────────────────────────────"
     )
    (global-org-modern-mode)))

(leaf *Global-Minnor-Mode
  :init
  (leaf goto-addr.el
    :doc "Toggle Goto-Address mode in all buffers."
    :url "https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html"
    :init
    ;; You can follow the URL by typing C-c RET
    (global-goto-address-mode 1)))

;; Load my settings
(leaf *Test-Bed
  :init
  ;; Do something
  )

(leaf *Load-My-Settings :disabled nil
  :init
  (my-tick-init-time "loading README.org")
  (org-babel-load-file "~/.emacs.d/README.org"))

(my-tick-init-time "end")
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
