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

(leaf *leaf
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

  (leaf leaf-convert :ensure t))

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
    (load bootstrap-file nil 'nomessage)))

;;;; Hi-priority packages
(leaf no-littering
  :url "https://github.com/emacscollective/no-littering#usage"
  :straight t :require t)

(leaf org :straight t)

(leaf macrostep ; to test leaf macros.
  :doc "interactive macro expander"
  :req "cl-lib-0.5"
  :tag "debugging" "macro" "languages" "lisp"
  :url "https://github.com/joddie/macrostep"
  :straight t
  :bind (("C-c e" . macrostep-expand)))
;;;; Packages

(leaf *lang-env
  :init
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (cond ((eq system-type 'windows-nt)
         (setq default-process-coding-system (cons 'utf-8 'cp932-unix)))))

(leaf *keyboard
  :init
  (define-key key-translation-map [?\C-h] [?\C-?])
  (global-set-key (kbd "C-^") help-map)
  (leaf *undo :bind (("C-z" . undo))))

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

(leaf *fonts-win
  :if (eq system-type 'windows-nt)
  :init
  ;; 通常使用するフォント
  (set-frame-font "PlemolJP-12" nil t)
  ;; 行間
  (setq-default line-spacing 0)
  ;; IME未確定時のフォント設定
  (modify-all-frames-parameters '((ime-font . "PlemolJP-12")))
  )

(leaf goto-addr.el
  :doc "Toggle Goto-Address mode in all buffers."
  :url "https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html"
  :init
  ;; You can follow the URL by typing C-c RET
  (global-goto-address-mode 1))

;; Load my settings
(leaf *my-settings :disabled nil
  :init
  (my-tick-init-time "loading README.org")
  (org-babel-load-file "~/.emacs.d/README.org"))

(my-tick-init-time "end")
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
