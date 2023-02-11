;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2023 Yoshihide Chubachi

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

(progn "Start Profiling"
  (defvar my/tick-previous-time (current-time))

  (defun my/tick-init-time (msg)
    "Tick boot sequence."
    (let ((ctime (current-time)))
      (message "--- %5.2f[ms] %s"
               (* 1000 (float-time
                        (time-subtract ctime my/tick-previous-time)))
               msg)
      (setq my/tick-previous-time ctime)))

  (my/tick-init-time "start"))

;;;; Package Management Tools
(eval-and-compile
  (prog1 "package"
    (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize))

  (prog1 "straight.el"
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

  (prog1 "leaf"
    (straight-use-package 'leaf)
    (straight-use-package 'leaf-keywords)
    (leaf-keywords-init))

  (leaf Leaf
    :init
    (leaf leaf-tree
      :straight t
      :custom (imenu-list-position . 'left)
      :init
      (defun my/enable-init-el-minor-mode ()
        (when (equal
               (buffer-file-name)
               (expand-file-name "~/.emacs.d/init.el"))
          (leaf-tree-mode t)))
      (add-hook 'find-file-hook 'my/enable-init-el-minor-mode))

  (leaf leaf-convert :straight t)))

(leaf Hi-Priority-Packages
  :init
  (leaf no-littering
    :url "https://github.com/emacscollective/no-littering#usage"
    :straight t :require t)

  (leaf org :straight t))

(leaf Builtin-Customizations
  :init
  (leaf Custom-File
    :init
    (custom-set-variables
     '(custom-file
       (no-littering-expand-etc-file-name "custom.el"))))

  (leaf Builtin-Variables
    :custom
    (inhibit-startup-screen . t)          ; スタートアップスクリーンを非表示
    (ring-bell-function . 'ignore)        ; ベルを鳴らさない
    (fill-column . 80)                    ; 80桁で改行（モードによる）
    (indent-tabs-mode . nil)              ; インデントの際タブを使わない
    (byte-compile-warnings
     . '(not cl-functions obsolete))      ; (require 'cl)を検査しない
    (epg-pinentry-mode . 'loopback)       ; GnuPGのパスフレーズをミニバッファで
    (plstore-cache-passphrase-for-symmetric-encryption . t)
                                          ; パスフレーズをキャッシュ
    (select-active-regions . 'only)       ; リージョン選択時の移動を早くする
    )

  (leaf Global-Minnor-Mode
    :init
    (leaf goto-addr.el
      :doc "Toggle Goto-Address mode in all buffers."
      :url "https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html"
      :init
      ;; You can follow the URL by typing C-c RET
      (global-goto-address-mode 1)))

  (leaf Keyboard-Settings
    :init
    (leaf Help-Keys
      :init
      (define-key key-translation-map [?\C-h] [?\C-?])
      (global-set-key (kbd "C-^") help-map))

    (leaf Global-Bindings
      :bind (("C-z" . undo)))))

(leaf Japanese-Settings
  :init
  (leaf *Language-Environment
    :init
    (set-language-environment "Japanese")
    (prefer-coding-system 'utf-8)
    (cond ((eq system-type 'windows-nt)
           (setq default-process-coding-system (cons 'utf-8 'cp932-unix)))))

  (leaf mozc
    :straight t
    :defvar (mozc-helper-program-name)
    :init
    (cond ((eq system-type 'windows-nt)
           (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper.exe"))
          (t
           (setq mozc-helper-program-name "mozc_emacs_helper")))

    (leaf mozc-im
      :straight t
      :require t
      :custom (default-input-method . "japanese-mozc-im")
      :bind* (("C-o" . toggle-input-method))
      :defvar (mozc-candidate-style)
      :init
      (setq mozc-candidate-style 'echo-area))

    (leaf mozc-cursor-color
      :straight (mozc-cursor-color :type git :host github
                                   :repo "iRi-E/mozc-el-extensions")
      :require t
      :defvar (mozc-cursor-color-alist) ;; FIXME: defvar-localが原因
      :config
      (setq mozc-cursor-color-alist
            '((direct        . "gray")
              (read-only     . "yellow")
              (hiragana      . "green")
              (full-katakana . "goldenrod")
              (half-ascii    . "dark orchid")
              (full-ascii    . "orchid")
              (half-katakana . "dark goldenrod")))

      (prog1 "mozc-cursor-color"
        ;; mozc-cursor-color を利用するための対策（NTEmacs@ウィキより）
        ;; https://w.atwiki.jp/ntemacs/?cmd=word&word=cursor-color&pageid=48
        (defvar-local mozc-im-mode nil) ;; FIXME: トップレベルじゃないと警告
        (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
        (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
        (advice-add 'mozc-cursor-color-update
                    :around (lambda (orig-fun &rest args)
                              (let ((mozc-mode mozc-im-mode))
                                (apply orig-fun args))))))

    (leaf *mozc-win
      :if (eq system-type 'windows-nt)
      :defun (mozc-session-sendkey)
      :init
      (advice-add 'mozc-session-execute-command
  	          :after (lambda (&rest args)
  	                   (when (eq (nth 0 args) 'CreateSession)
  	                     (mozc-session-sendkey '(Hankaku/Zenkaku))))))))

(leaf Main
  :disabled nil
  :init
  (leaf My-Functions
    :init
    (leaf Fonts
      :init
      (defun my/list-available-fonts ()
        (interactive)
        (let ((font-list (font-family-list)))
          (with-output-to-temp-buffer "*Available Fonts*"
            (set-buffer "*Available Fonts*")
            (dolist (font font-list)
              (insert (format "%s\n" font)))
            (goto-char (point-min)))))

      (defun my/font-exists-p (font-name)
        (if (null (x-list-fonts font-name))
            nil t))))

  (leaf Look-And-Feel
    :custom
    (line-spacing . 0.2)
    :init
    (leaf Fonts
      :doc "フォント設定。確認はC-u C-x =。"
      :when (display-graphic-p)
      :init
      ;; ｜あいうえお｜
      ;; ｜憂鬱な檸檬｜
      ;; ｜<miilwiim>｜
      ;; ｜!"#$%&'~{}｜
      ;; ｜🙆iimmiim>｜
      (let (
            ;; (font-name "Noto Sans Mono-11")
            ;; (font-name "PlemolJP-11") ; IBM Plex Sans JP + IBM Plex Mono
            (font-name "HackGen-11")    ;  源ノ角ゴシックの派生 + Hack
            ;; (font-name "UDEV Gothic NF-12") ; BIZ UDゴシック + JetBrains Mono
            ;; (font-name "FirgeNerd-11") ; 源真ゴシック + Fira Mono
            )
        (if (null (x-list-fonts font-name))
            (error (format "No such font: %s" font-name)))
        (set-face-attribute 'default nil :font font-name)))

    (leaf modus-themes
      :straight t                       ; omit this to use the built-in themes
      :custom
      (modus-themes-italic-constructs . nil)
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
       ;; Edit settings
       org-auto-align-tags nil ; Non-nil keeps tags aligned when modifying headlines.
       org-tags-column 0
       org-catch-invisible-edits 'show-and-error
       org-special-ctrl-a/e t

       ;; Org styling, hide markup etc.
       org-hide-emphasis-markers t
       org-pretty-entities t
       ;; org-ellipsis "…"

       ;; Agenda styling
       org-agenda-tags-column 0
       org-agenda-block-separator ?─
       org-agenda-time-grid
       '((daily today require-timed)
         (800 1000 1200 1400 1600 1800 2000)
         " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
       org-agenda-current-time-string
       "⭠ now ─────────────────────────────────────────────────"
       )
      (global-org-modern-mode)))

  (leaf Org-Mode
    :init
    (leaf Org-Settings
      :bind
      (("C-c l" . org-store-link)
       ("C-c a" . org-agenda)
       ("C-c c" . org-capture))
      :custom
      (org-directory . "~/Dropbox/Org/")
      (org-default-notes-file . "~/Dropbox/Org/Notebook.org")
      (org-agenda-files . '("~/Dropbox/Org/"))
      (org-todo-keyword-faces
       . '(("NEXT" . (:foreground "blue" :underline t))
           ("DONE" . (:foreground "pale green"))))
      (org-todo-keywords . '((sequence "TODO" "NEXT" "|" "DONE" "SOMEDAY")))
      (org-refile-targets . '((org-agenda-files :tag . "REFILE")))
      (org-startup-truncated . nil)
      (org-return-follows-link  . t)        ; RET/C-mでリンクを開く
      (org-agenda-start-with-follow-mode . t) ; アジェンダで関連するorgファイルを開く
      (org-ellipsis . " ▽")                  ; …,▼, ↴, ⬎, ⤷, ⋱
      (org-export-with-sub-superscripts . nil) ; A^x B_z のような添字の処理をしない
      (org-agenda-remove-tags . t)             ; アジェンダにタグを表示しない
      (org-id-link-to-org-use-id . 'create-if-interactive-and-no-custom-id)
      )
    )

  (leaf macrostep                       ; to test leaf macros.
    :doc "interactive macro expander"
    :url "https://github.com/joddie/macrostep"
    :straight t
    :bind (("C-c e" . macrostep-expand)))

  (leaf warnings
    :custom
    (warning-suppress-types . '(((yasnippet backquote-change))
                                (org-element-cache))))

  (leaf moody
    :straight t
    :config
    (setq x-underline-at-descent-line t)
    (moody-replace-mode-line-buffer-identification)
    (moody-replace-vc-mode)
    (moody-replace-eldoc-minibuffer-message-function))

  (leaf minions
    :straight t
    :require t
    :config
    (minions-mode 1)
    (setq minions-mode-line-lighter "[+]")
    (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))

  (leaf beacon
    :straight t
    :custom
    (beacon-blink-when-focused . nil)
    :config
    (beacon-mode 1))

  (leaf whitespace
    :require 't
    :config
    (setq whitespace-style
          '(
            face                        ; faceで可視化
            trailing                    ; 行末
            tabs                        ; タブ
            spaces                      ; スペース
            space-mark                  ; 表示のマッピング
            tab-mark
            ))
    (setq whitespace-display-mappings
          '(
            (space-mark ?\u3000 [?□])
            (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
            ))
    (setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
    (setq whitespace-space-regexp "\\(\u3000+\\)")
    ;; (set-face-attribute 'whitespace-trailing nil
    ;;                     :foreground nil
    ;;                     :background "DarkOrange1"
    ;;                     :underline nil)
    ;; (set-face-attribute 'whitespace-tab nil
    ;;                     :foreground "DarkOrange1"
    ;;                     :background nil
    ;;                     :underline nil)
    ;; (set-face-attribute 'whitespace-space nil
    ;;                     :foreground "DarkOrange1"
    ;;                     :background nil
    ;;                     :underline nil)
    (global-whitespace-mode t))

  (leaf all-the-icons :straight t)

  (leaf ruler-mode
    :config
    (add-hook 'find-file-hook (lambda () (ruler-mode 1))))

  (leaf *emacs
    (defalias 'yes-or-no-p 'y-or-n-p))

  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (global-set-key (kbd "M-SPC") 'cycle-spacing)

  (leaf dired
    :custom
    (dired-dwim-target . t))

  (leaf vc-hooks
    :custom
    (vc-follow-symlinks . t)            ; シンボリックリンクの場合、本体を辿る
    (vc-handled-backends . '(Git)))     ; Gitのみ使用

  (leaf tramp
    :config
    (defadvice tramp-sh-handle-vc-registered (around tramp-sh-handle-vc-registered activate)
      (let ((vc-handled-backends nil)) ad-do-it)))

  (leaf files
    :custom
    (backup-directory-alist . '(("." . ".backup~")))
    (delete-old-versions . t)
    (version-control . t)
    (auto-save-file-name-transforms
     . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

  (leaf display-fill-column-indicator
    :hook
    (emacs-startup-hook . global-display-fill-column-indicator-mode))

  (leaf display-line-numbers
    :custom
    (display-line-numbers-width . 5)    ; 表示する行番号の桁数
    :hook
    (emacs-startup-hook . global-display-line-numbers-mode))

  (leaf save-place
    :custom
    (save-place . t)
    :hook
    (emacs-startup-hook . save-place-mode))

  (leaf auto-revert
    :custom
    (auto-revert-interval . 1)          ; 再読み込みの間隔
    (auto-revert-verbose . nil)         ; 再読込の際、メッセージを非表示
    (auto-revert-check-vc-info . t)     ; VCで更新があった場合、自動で更新
    :hook
    (emacs-startup-hook . global-auto-revert-mode))

  (leaf paren
    :custom
    (show-paren-style . 'mixed)
    :hook
    (emacs-startup-hook . show-paren-mode))

  (leaf isearch
    :bind ((isearch-mode-map
            ("C-o" . isearch-toggle-input-method))))

  (leaf wdired
    :doc "Rename files editing their names in dired buffers"
    :tag "builtin"
    :added "2020-11-21"
    :require t
    :config
    (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
    :bind ((wdired-mode-map
            ("C-o" . toggle-input-method))))

  (leaf recentf
    :custom
    (recentf-max-menu-items  . 25)
    (recentf-max-saved-items . 2000)
    (recentf-auto-cleanup    . 'never)
    (recentf-exclude . '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:"))
    :hook
    (emacs-startup-hook . recentf-mode)
    :config
    (run-at-time nil (* 5 60)
                 (lambda ()
                   (let ((save-silently t)) ; FIXME
                     (recentf-save-list))))
    (leaf no-littering
      :after no-littering
      :config
      (add-to-list 'recentf-exclude no-littering-var-directory)
      (add-to-list 'recentf-exclude no-littering-etc-directory)))

  (leaf midnight
    :custom
    ((clean-buffer-list-delay-general . 1))
    :hook
    (emacs-startup-hook . midnight-mode))

  (leaf *imenu-list
    :bind (("C-c i" . imenu-list-smart-toggle))
    :hook (imenu-list-major-mode-hook . (lambda nil (display-line-numbers-mode -1))))

  (leaf outline-minor-mode
    :config
    (add-hook 'outline-minor-mode-hook
              (lambda () (local-set-key "\C-c\C-o"
                                        outline-mode-prefix-map))))

  (leaf ffap
    :config
    (ffap-bindings))

  (leaf frame
    :bind ("<f11>" . toggle-frame-maximized))

  (leaf vertico
    :straight t
    :custom
    ;; 最大20件まで表示するように
    (vertico-count . 20)
    :config
    (vertico-mode)
    (setq vertico-resize t)
    (setq vertico-cycle t)
    )

  (leaf orderless
    :straight t
    :init
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (setq orderless-style-dispatchers '(+orderless-dispatch))
    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion)))))

  ;; Persist history over Emacs restarts. Vertico sorts by history position.
  (leaf savehist
    :straight t
    :init
    (savehist-mode))

  (leaf marginalia
    :straight t
    :bind (:minibuffer-local-map
           ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  (leaf embark
    :straight t
    :bind
    (("M-." . embark-act)
     ("C-." . embark-dwim)
     ("C-^ B" . embark-bindings) ;; C-h -> C-^ にしています
     )
    :init
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))
    :config
    (leaf *my-embark-orglink
      :after org embark
      :config
      (defun my-embark-orglink-at-point ()
        "Target a link at point of orglink."
        (save-excursion
          (let* ((cur (point))
                 (beg (progn (search-backward "[" nil t) (point)))
                 (end (progn (search-forward  "]" nil t) (point)))
                 (str (buffer-substring-no-properties beg end)))
            (when (and (<= beg cur) (<= cur end))
              (save-match-data
                (when (string-match "\\(\\[.+\\]\\)" str)
                  `(orglink
                    ,(format "%s" (match-string 1 str))
                    ,beg . ,end)))))))
      (add-to-list 'embark-target-finders 'my-embark-orglink-at-point)
      (embark-define-keymap embark-orglink-map
                            "Orglink keymap"
                            ("RET" org-open-at-point)
                            ("o" org-open-at-point))
      (add-to-list 'embark-keymap-alist '(orglink . embark-orglink-map))))

  ;; Example configuration for Consult
  (leaf consult
    :straight (consult :type git :host github
                       :repo "minad/consult")
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
           ;; ("C-c h" . consult-history)
           ;; ("C-c m" . consult-mode-command)
           ;; ("C-c k" . consult-kmacro)
           ;; C-x bindings (ctl-x-map)
           ("C-x M-:"  . consult-complex-command) ;; orig. repeat-complex-command
           ("C-x b"    . consult-buffer)          ;; orig. switch-to-buffer
           ("C-x 4 b"  . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b"  . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
           ("C-x r b"  . consult-bookmark)           ;; orig. bookmark-jump
           ;; ("C-x p b"  . consult-project-buffer)      ;; orig. project-switch-to-buffer
           ("M-#"      . consult-register-load) ;; Custom M-# bindings for fast register access
           ("M-'"      . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#"    . consult-register)
           ;; Other custom bindings
           ("M-y"      . consult-yank-pop) ;; orig. yank-pop
           ("<help> a" . consult-apropos)  ;; orig. apropos-command
           ;; M-g bindings (goto-map)
           ("M-g e"    . consult-compile-error)
           ("M-g f"    . consult-flymake)   ;; Alternative: consult-flycheck
           ("M-g g"    . consult-goto-line) ;; orig. goto-line
           ("M-g M-g"  . consult-goto-line) ;; orig. goto-line
           ("M-g o"    . consult-outline)   ;; Alternative: consult-org-heading
           ("M-g m"    . consult-mark)
           ("M-g k"    . consult-global-mark)
           ("M-g i"    . consult-imenu)
           ("M-g I"    . consult-imenu-multi)
           ;; M-s bindings (search-map)
           ("M-s d"    . consult-find)
           ("M-s D"    . consult-locate)
           ("M-s g"    . consult-grep)
           ("M-s G"    . consult-git-grep)
           ("M-s r"    . consult-ripgrep)
           ("M-s l"    . consult-line)
           ("M-s L"    . consult-line-multi)
           ("M-s m"    . consult-multi-occur)
           ("M-s k"    . consult-keep-lines)
           ("M-s u"    . consult-focus-lines)
           ;; Isearch integration
           ("M-s e"    . consult-isearch-history)
           (:isearch-mode-map
            ("M-e"     . consult-isearch-history) ;; orig. isearch-edit-string
            ("M-s e"   . consult-isearch-history) ;; orig. isearch-edit-string
            ("M-s l"   . consult-line) ;; needed by consult-line to detect isearch
            ("M-s L"   . consult-line-multi))) ;; needed by consult-line to detect isearch

    ;; Enable autom  atic preview at point in the *Completions* buffer. This is
    ;; relevant when you use the default completion UI.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0.5
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Optionally replace `completing-read-multiple' with an enhanced version.
    (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-bookmark consult--source-recent-file
     consult--source-project-recent-file
     :preview-key (kbd "M-."))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; By default `consult-project-function' uses `project-root' from project.el.
    ;; Optionally configure a different project root function.
    ;; There are multiple reasonable alternatives to chose from.
    ;; 1. project.el (the default)
    ;; (setq consult-project-function #'consult--default-project--function)
    ;; 2. projectile.el (projectile-project-root)
    (autoload 'projectile-project-root "projectile")
    (setq consult-project-function (lambda (_) (projectile-project-root)))
    ;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
    ;; 4. locate-dominating-file
    ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
    )

  ;; Consult users will also want the embark-consult package.
  (leaf embark-consult
    :straight t
    :after (embark consult)
    ;; :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

  (leaf doct
    :straight t
    ;;recommended: defer until calling doct
                                        ;:commands (doct)
    :config
    (setq org-capture-templates
          (doct '(("Memo" :keys "m"
                   :empty-lines-after 1
                   :file "~/Dropbox/Org/Memo.org"
                   :datetree t
                   :unnarrowed nil       ; t
                   :jump-to-captured nil ;
                   :empty-lines-before 1
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

  (leaf org-tempo
    :require t
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
    )

  (leaf ox-latex
    :require t
    :custom
    (org-latex-compiler      . "lualatex")
    (org-latex-pdf-process   . '("latexmk -f -gg -pvc- -%latex %f"))
    (org-latex-default-class . "jlreq")
    (org-latex-hyperref-template
     . "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},pdfsubject={%d},\n pdfcreator={%c},\n pdflang={Japanese},\n colorlinks={true},linkcolor={blue}\n}\n")
    (org-latex-listings . 'minted)
    (org-latex-minted-options
     . '(("frame" "lines")
         ("framesep=2mm")
         ("linenos=true")
         ("baselinestretch=1.2")
         ("fontsize=\\footnotesize")
         ("breaklines")))
    :config
    (add-to-list
     'org-latex-classes
     '("jlreq"
       "\\documentclass{jlreq}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
    (add-to-list
     'org-latex-classes
     '("jlreq-tate"
       "\\documentclass[tate]{jlreq}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
    (add-to-list
     'org-latex-classes
     '("bxjsarticle"
       "\\documentclass{bxjsarticle}\n\\usepackage{luatexja}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
       ("\\paragraph{%s}"     . "\\paragraph*{%s}")
       ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
    (add-to-list
     'org-latex-classes
     '("beamer"
       "\\documentclass[presentation]{beamer}\n\\usepackage{luatexja}\n\\renewcommand\\kanjifamilydefault{\\gtdefault}"
       ("\\section{%s}"       . "\\section*{%s}")
       ("\\subsection{%s}"    . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

    (add-to-list 'org-latex-packages-alist
                 "\\usepackage{minted}" t)

    (leaf ox-beamer
      :require t
      :custom
      (org-beamer-outline-frame-title . "目次")
      (org-beamer-frame-default-options . "t")))

  (leaf *org-babel
    :config
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((java . t) (ruby . t) (python . t) (C . t) (dot . t)))
    (setq org-confirm-babel-evaluate nil)
    (eval-after-load 'org
      (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images))
    (nconc org-babel-default-header-args:java
           '((:dir . nil)
             (:results . "value"))))

  (leaf *org-use-speed-commands
    :config
    (setq org-use-speed-commands
          (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**")))))

  (leaf org2blog
    :straight t
    :config
    (require 'auth-source)
    (let* ((credentials (auth-source-user-and-password "ploversky.net"))
           (username (nth 0 credentials))
           (password (nth 1 credentials))
           (config `("plover"
                     :url "https://ploversky.net/xmlrpc.php"
                     :username ,username
                     :password ,password)))
      (setq org2blog/wp-blog-alist `(,config)))
    (setq org2blog/wp-image-upload t)
    (setq org2blog/wp-show-post-in-browser 'show)
    (setq org2blog/wp-use-sourcecode-shortcode t)
    )

  (leaf ox-hugo
    :straight t
    :require t
    :after ox)

  (leaf org-superstar
    :straight t
    :config
    (add-hook 'org-mode-hook (lambda nil (org-superstar-mode 1)))
    (setq org-superstar-headline-bullets-list
          '("●" "■" "▷" "○")))

  (leaf ox-zenn
    :straight t
    :after org
    :require t ox-publish
    :defun zenn/f-parent org-publish
    :defvar org-publish-project-alist
    :preface
    (defvar zenn/org-dir "~/git/zenn-content")

    (defun zenn/org-publish (arg)
      "Publish zenn blog files."
      (interactive "P")
      (let ((force (or (equal '(4) arg) (equal '(64) arg)))
            (async (or (equal '(16) arg) (equal '(64) arg))))
        (org-publish "zenn" arg force async)))

    :config
    (setf
     (alist-get "zenn" org-publish-project-alist nil nil #'string=)
     (list
      :base-directory (expand-file-name "" zenn/org-dir)
      :base-extension "org"
      :publishing-directory (expand-file-name "../" zenn/org-dir)
      :recursive t
      :publishing-function 'org-zenn-publish-to-markdown)))

  (leaf ox-pandoc :straight t :require t)

  (leaf org-pomodoro
    :straight t
    :require t)

  (leaf org-contrib
    :straight t
    :config
    (require 'ox-taskjuggler))

  (leaf *org-publish-project-alist
    :config
    (setq org-publish-project-alist
          '(("chubachi.net"
             :components ("chubachi.net-orgfiles" "chubachi.net-others"))

            ("chubachi.net-orgfiles"
             :publishing-function org-html-publish-to-html
             :base-directory "~/Dropbox/Org/publish/chubachi.net/"
             :publishing-directory "/scpx:chubachi@chubachi.sakura.ne.jp:~/www/chubachi.net/publish"
             :base-extension "org"
             :recursive t
             ;; options for html files
             ;; :exclude "PrivatePage.org" ;; regexp
             ;; :headline-levels 3
             ;; :section-numbers nil
             ;; :with-toc nil
             ;; :html-head "<link rel=\"stylesheet\" type=\"text/css\"
             ;;    href=\"https://gongzhitaao.org/orgcss/org.css\"/>"
             ;;:html-preamble t
             )

            ("chubachi.net-others"
             :publishing-function org-publish-attachment
             :base-directory "~/Dropbox/Org/publish/chubachi.net/"
             :publishing-directory "/scpx:chubachi@chubachi.sakura.ne.jp:~/www/chubachi.net/publish/"
             :base-extension "jpg\\|gif\\|png|css\\|el"
             :recursive t))))

  ;; plantuml.jarへのパスを設定
  (setq org-plantuml-jar-path "~/.emacs.d/lib/plantuml-1.2022.12.jar")

  ;; org-babelで使用する言語を登録
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))

  (leaf org-download
    :straight t
    :require t
    :custom (org-download-method . 'attach)
    :config
    (setq org-image-actual-width 400)   ; width of images (#+ATTR_ORG: 400)
    (add-hook 'dired-mode-hook 'org-download-enable)
    (leaf
      :when (eq system-type 'windows-nt)
      :custom (org-download-screenshot-method . "magick convert clipboard: %s")))

  (leaf which-key
    :doc "Display available keybindings in popup"
    :req "emacs-24.4"
    :tag "emacs>=24.4"
    :url "https://github.com/justbur/emacs-which-key"
    :added "2021-10-20"
    :emacs>= 24.4
    :straight t
    :config
    (which-key-mode))

  (leaf swap-buffers
    :straight t
    :bind
    ("C-c b" . swap-buffers)
    :custom
    (swap-buffers-qwerty-shortcuts
     . '("a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-")))

  (leaf backup-each-save
    :when (not (eq system-type 'windows-nt))
    :straight t
    :config
    ;; バックアップ先
    (setq backup-each-save-mirror-location "~/.emacs.d/backups")
    ;; バックアップファイルにつけるsuffix
    (setq backup-each-save-time-format "%y%m%d_%H%M%S")
    ;; バックアップするファイルサイズの上限
    (setq backup-each-save-size-limit 5000000)
    ;; すべてのファイルをバックアップする
    (setq backup-each-save-filter-function 'identity)
    ;; 有効化！
    (add-hook 'after-save-hook 'backup-each-save))

  (leaf undo-tree
    :straight t
    :custom
    (undo-tree-auto-save-history . t)
    :config
    (global-undo-tree-mode))

  (leaf pandoc-mode
    :doc "Minor mode for interacting with Pandoc"
    :req "hydra-0.10.0" "dash-2.10.0"
    :tag "pandoc" "text"
    :added "2020-11-24"
    :url "http://joostkremers.github.io/pandoc-mode/"
    :straight t
    :after hydra)

  (leaf magit
    :doc "A Git porcelain inside Emacs."
    :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
    :tag "vc" "tools" "git" "emacs>=25.1"
    :added "2020-11-30"
    :emacs>= 25.1
    ;; :straight t
    :straight t
    :after git-commit with-editor
    :bind (("C-x g" . magit-status))
    )

  (leaf git-gutter
    :straight t
    ;; :custom
    ;; (git-gutter:modified-sign . "~")
    ;; (git-gutter:added-sign    . "+")
    ;; (git-gutter:deleted-sign  . "-")
    ;; :custom-face
    ;; (git-gutter:modified . ((t (:background "#f1fa8c"))))
    ;; (git-gutter:added    . ((t (:background "#50fa7b"))))
    ;; (git-gutter:deleted  . ((t (:background "#ff79c6"))))
    :hook
    (emacs-startup-hook . global-git-gutter-mode))

  (leaf migemo
    :when (eq system-type 'gnu/linux)
    :straight t
    :require t ; Checked on 2023-02-12
    :custom
    (migemo-command . "cmigemo")
    (migemo-options .'("-q" "--emacs"))
    (migemo-dictionary . "/usr/share/cmigemo/utf-8/migemo-dict")
    (migemo-user-dictionary . nil)
    (migemo-regex-dictionary . nil)
    (migemo-coding-system . 'utf-8-unix)
    :defun (migemo-init)
    :config
    (migemo-init))

  (leaf migemo
    :when (and
           (eq system-type 'windows-nt)
           (file-exists-p "C:/Users/yc/lib/cmigemo-default-win64/dict/utf-8/migemo-dict"))
    :straight t
    :custom
    (migemo-dictionary . "C:/Users/yc/lib/cmigemo-default-win64/dict/utf-8/migemo-dict")
    :config
    (load-library "migemo")
    (migemo-init))

  (leaf yasnippet-snippets
    :straight t
    :custom
    (yasnippet-snippets-dir . "~/.emacs.d/etc/yasnippet/snippets")
    :hook
    (emacs-startup-hook . yas-global-mode))

  (leaf yaml-mode :straight t)

  (leaf shell-pop
    :straight t
    :bind
    ("C-c s" . shell-pop)
    :custom
    (shell-pop-shell-type . (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
    (shell-pop-window-position . "bottom")
    (setq shell-pop-full-span . t))

  (leaf visual-fill-column
    :doc "fill-column for visual-line-mode"
    :req "emacs-25.1"
    :tag "emacs>=25.1"
    :url "https://github.com/joostkremers/visual-fill-column"
    :added "2021-11-08"
    :emacs>= 25.1
    :straight t
    :after org-mode
    :hook (org-mode-hook . visual-fill-column-mode)
    :bind(("C-c q" . visual-fill-column-mode)
          (:visual-fill-column-mode-map
           ("C-a" . beginning-of-visual-line)
           ("C-e" . end-of-visual-line)
           ("C-k" . kill-visual-line))))

  (leaf dired-launch
    :straight t
    :hook (dired-mode-hook . dired-launch-mode)
    :config
    (when (and (eq system-type 'gnu/linux)
               (getenv "WSLENV"))
      (setq dired-launch-default-launcher '("wslview"))))

  (leaf google-this
    :straight t
    :config
    (google-this-mode 1))

  (leaf region-bindings-mode
    :straight t
    :require t
    :config
    (region-bindings-mode-enable))

  (leaf multiple-cursors
    :straight t
    :after region-bindings-mode
    :config
    (define-key region-bindings-mode-map "e" 'mc/edit-lines)
    (define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
    (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
    (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
    (define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended))

  (leaf align
    :doc "align text to a specific column, by regexp"
    :tag "builtin"
    :added "2021-10-30"
    :after region-bindings-mode
    :config
    (define-key region-bindings-mode-map "=" 'align-regexp))

  (setq user-full-name "Yoshihide Chubachi")
  (setq user-mail-address "yc@aiit.ac.jp")

  (setq mail-user-agent 'message-user-agent)

  (setq message-send-mail-function 'smtpmail-send-it)

  (setq smtpmail-stream-type 'starttls
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (leaf eww
    :custom
    (
     ;; (browse-url-browser-function . 'eww-browse-url)
     (shr-use-colors    . nil)
     (shr-use-fonts     . nil)
     (shr-image-animate . nil)
     (shr-width         . 72)
     (eww-search-prefix . "https://www.google.com/search?q=")
     )
    )

  (leaf notmuch
    :disabled nil
    ;; :straight t
    :straight t
    :require t
    :hook
    (notmuch-message-mode-hook . visual-fill-column-mode)
    (notmuch-message-mode-hook . (lambda () (auto-fill-mode -1)))
    :custom
    ((notmuch-draft-folder . "/drafts") ; 編集中のドラフトはローカルのフォルダに
     (notmuch-fcc-dirs . nil)           ; 送信済みメールはローカルに保存せず
                                        ; Gmailに任せる
     (notmuch-search-oldest-first . nil) ; 検索結果を新しい順でソート
     (notmuch-saved-searches
      . '((:name "flagged"    :query "tag:flagged AND NOT tag:deleted"
                 :key "f" :search-type tree)
          (:name "inbox"      :query "tag:inbox folder:/Gmail\\/inbox/ AND NOT tag:deleted"
                 :key "i" :search-type tree)
          (:name "unread"     :query "tag:unread AND NOT tag:deleted"
                 :key "u" :search-type tree)
          (:name "sent"       :query "tag:sent AND NOT tag:deleted"
                 :key "s" :search-type tree)
          (:name "drafts"     :query "tag:draft AND NOT tag:deleted"
                 :key "d" :search-type tree)
          (:name "Gmal Inbox" :query "folder:/Gmail\\/inbox/"
                 :key "I" :search-type tree)
          (:name "Gmal Sent"  :query "folder:/Gmail\\/sent/"
                 :key "S" :search-type tree)
          (:name "all mail"   :query "NOT tag:deleted"
                 :key "a" :search-type tree)))
     )
    :bind (("C-c r" . notmuch-hello))
    :config
    (advice-add #'notmuch-read-tag-changes
                :filter-return (lambda (x) (mapcar #'string-trim x))) ; vertico対策
    :config
    (define-key notmuch-search-mode-map "f"
      (lambda ()
        "toggle flaged tag for message"
        (interactive)
        (if (member "flagged" (notmuch-search-get-tags))
            (notmuch-search-tag (list "-flagged"))
          (notmuch-search-tag (list "+flagged")))))
    (define-key notmuch-show-mode-map "f"
      (lambda ()
        "toggle flaged tag for message"
        (interactive)
        (if (member "flagged" (notmuch-show-get-tags))
            (notmuch-show-tag (list "-flagged"))
          (notmuch-show-tag (list "+flagged")))))
    (define-key notmuch-tree-mode-map "f"
      (lambda ()
        "toggle flaged tag for message"
        (interactive)
        (if (member "flagged" (notmuch-tree-get-tags))
            (notmuch-tree-tag (list "-flagged"))
          (notmuch-tree-tag (list "+flagged"))))))

  (leaf ol-notmuch
    :straight t
    :require t
    :after notmuch org)

  (leaf consult-notmuch
    ;; :straight (consult-notmuch :type git :host github
    ;;                            :repo "emacsmirror/consult-notmuch")
    :straight t
    :after consult notmuch)

  (leaf mm-decode
    :custom (mm-default-directory . "~/Downloads/"))

  (leaf gnus-alias
    :straight (gnus-alias :type git :host github
                          :repo "hexmode/gnus-alias")
    :config
    (setq gnus-alias-identity-alist
          '(("work"
             nil
             "中鉢欣秀 <yc@aiit.ac.jp>"
             nil            ;; No organization header
             nil            ;; No extra headers
             nil            ;; No extra body text
             "~/.signature" ;; My signature
             ))))

  (leaf *cmd/browser
    :config
    (defun cmd/wsl-browser (url &rest ignore)
      "Browse URL using wslview."
      (interactive "sURL: ")
      (shell-command (concat "wslview " "'" url "'")))

    (when (and (eq system-type 'gnu/linux)
               (getenv "WSLENV"))
      (setq browse-url-browser-function 'cmd/wsl-browser)
      (setq browse-url-generic-program "web-browser")))

  (leaf highlight-indent-guides
    :straight t
    :require t
    :hook
    ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-auto-enabled . t)
    (highlight-indent-guides-responsive . t)
    (highlight-indent-guides-method . 'column) ;fill)) ;character)) ; column
    )

  (leaf rainbow-delimiters
    :straight t
    :hook
    (prog-mode-hook . rainbow-delimiters-mode))

  (leaf company
    :straight t
    :hook (after-init-hook . global-company-mode)
    :config
    (defun my-company-inhibit-idle-begin ()
      (setq-local company-begin-commands nil))
    (add-hook 'org-mode-hook #'my-company-inhibit-idle-begin)
    (add-hook 'text-mode-hook #'my-company-inhibit-idle-begin))

  (leaf flycheck
    :doc "On-the-fly syntax checking"
    :emacs>= 24.3
    :straight t
    :bind (("M-n" . flycheck-next-error)
           ("M-p" . flycheck-previous-error))
    :custom ((flycheck-emacs-lisp-initialize-packages . t)
             (flycheck-disabled-checkers . '(emacs-lisp-checkdoc)))
    :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
    :config
    (leaf flycheck-package
      :doc "A Flycheck checker for elisp package authors"
      :straight t
      :config
      (flycheck-package-setup))

    (leaf flycheck-elsa
      :doc "Flycheck for Elsa."
      :emacs>= 25
      :straight t
      :config
      (flycheck-elsa-setup))
    )

  (leaf projectile
    :straight t
    :require t
    :bind ((projectile-mode-map
            ("C-x p" . projectile-command-map))
           (projectile-command-map
            ("b" . consult-project-buffer)))
    :config
    (setq projectile-project-search-path '("~/.emacs.d/" ("~/git" . 1)))
    (projectile-mode 1))

  (leaf perspective
    :straight t
    :require t
    :custom
    (persp-mode-prefix-key . "p")
    :bind (;; (persp-mode-map
           ;;  ("C-c p" . perspective-map))
           ("C-x C-b" . persp-list-buffers))
    :config
    (persp-mode)
    (leaf consult
      :straight t
      :require t
      :config
      (consult-customize consult--source-buffer :hidden t :default nil)
      (add-to-list 'consult-buffer-sources persp-consult-source)))

  (leaf coverage :straight t)

  (leaf dockerfile-mode :straight t
    :config
    ;; Set dockerfile-image-name as safe variable.
    (put 'dockerfile-image-name 'safe-local-variable #'stringp))

  (leaf paredit
    :straight t
    :commands enable-paredit-mode
    :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
           (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
           (ielm-mode-hook . enable-paredit-mode)
           (lisp-mode-hook . enable-paredit-mode)
           (lisp-interaction-mode-hook . enable-paredit-mode)
           (scheme-mode-hook . enable-paredit-mode)))

  (leaf *emacs-lisp-mode-hook
    :config
    (add-hook
     'emacs-lisp-mode-hook
     'outline-minor-mode))

  (leaf ert
    :bind (("C-c t" . cmd/run-ert))
    :config
    (defun cmd/run-ert ()
      (interactive)
      (eval-buffer)
      (call-interactively 'ert)))

  (leaf emacs-refactor
    :straight t
    :bind ((emacs-lisp-mode-map
            ("M-RET" . emr-show-refactor-menu))))

  (leaf popup :straight t)

  (leaf list-utils :straight t)

  (leaf iedit :straight t)

  (leaf wanderlust :straight t
    :config
    ;; IMAP
    (setq elmo-imap4-default-user "yc@aiit.ac.jp"
          elmo-imap4-default-authenticate-type 'clear
          elmo-imap4-default-server "imap.gmail.com"
          elmo-imap4-default-port 993
          elmo-imap4-default-stream-type 'ssl
          )
    ;; For non ascii-characters in folder-names
    (setq elmo-imap4-use-modified-utf7 t)

    ;; (setq elmo-plugged t)
    ;; (setq elmo-plugged-condition 'independent)

    ;; メッセージ受信の上限を無限にする
    (setq elmo-message-fetch-threshold nil)

    ;; SMTP
    (setq
     wl-smtp-connection-type   'starttls        ; Use TLS
     wl-smtp-authenticate-type "login"          ; Authentication type
     wl-smtp-posting-user      "yc@aiit.ac.jp"  ; Username
     wl-smtp-posting-server    "smtp.gmail.com" ; SMTP server
     wl-smtp-posting-port      587              ; The SMTP port

     wl-local-domain           "aiit.ac.jp"  ; The SMTP server again
     wl-message-id-domain      "aiit.ac.jp") ; And... Again?

    (setq
     wl-default-folder "%INBOX"
     wl-draft-folder   "%[Gmail]/下書き"
     wl-trash-folder   "%[Gmail]/ゴミ箱"

     wl-from "Yoshihide Chubachi <yc@aiit.ac.jp>" ; Our From: header field
     wl-fcc-force-as-read t      ; Mark sent mail (in the wl-fcc folder) as read
     wl-default-spec "%")        ; For auto-completion

    ;; 隠したいヘッダの設定
    (setq wl-message-ignored-field-list
          '("ARC-.*:" "X-.*:" ".*Received.*:"
            "Authentication-Results:" "MIME-Version:"
            "List-.*:" "DKIM-.*:"
            ".*Path:" ".*Id:" "^References:"
            "^Replied:" "^Errors-To:"
            "^Lines:" "^Sender:" ".*Host:" "^Xref:"
            "^Content-Type:" "^Precedence:"
            "^Status:" "^X-VM-.*:"))

    ;; 表示するヘッダの設定
    ;; 'wl-message-ignored-field-list' より優先される
    (setq wl-message-visible-field-list '("^Message-Id:"))

    ;; 大きいメッセージを送信時に分割しない
    (setq mime-edit-split-message nil)

    (require 'wl-qs)
    (setq wl-quicksearch-folder "%[Gmail]/すべてのメール")

    (add-to-list 'wl-dispose-folder-alist
                 '("^%INBOX" . remove))
    (add-to-list 'wl-dispose-folder-alist
                 '(".*Junk$" . remove))

    (require 'elmo nil 'noerror)
    (defun my:wl-summary-jump-to-referer-message ()
      (interactive)
      (when (wl-summary-message-number)
        (if (eq (elmo-folder-type-internal wl-summary-buffer-elmo-folder) 'flag)
            (progn
              (let* ((referer (elmo-flag-folder-referrer
                               wl-summary-buffer-elmo-folder
                               (wl-summary-message-number)))
                     (folder (if (> (length referer) 1)
                                 (completing-read
                                  (format "Jump to (%s): " (car (car referer)))
                                  referer
                                  nil t nil nil (car (car referer)))
                               (car (car referer)))))
                (wl-summary-goto-folder-subr folder 'no-sync nil nil t)
                (wl-summary-jump-to-msg (cdr (assoc folder referer)))))
          (when (eq (elmo-folder-type wl-summary-last-visited-folder) 'internal)
            (wl-summary-goto-last-visited-folder)))))
    (define-key wl-summary-mode-map "=" 'my:wl-summary-jump-to-referer-message)
    )

  (leaf files+
    :straight t)

  (leaf ls-lisp+
    :straight t)

  (leaf w32-browser
    :straight t)

  (leaf dired+
    :straight (dired+ :type git :host github
                      :repo "emacsmirror/dired-plus"))

  (leaf org-sync-gtasks
    ;; :straight (org-sync-gtasks :type git :host github
    ;;                            :repo "ychubachi/org-sync-gtasks"
    ;;                            :branch "develop")
    :init
    (leaf oauth2 :straight t)
    :config
    (setq load-path (cons "~/git/org-sync-gtasks" load-path))
    (require 'org-sync-gtasks))

  (leaf org-sync-qiita
    ;; :straight (org-sync-gtasks :type git :host github
    ;;                            :repo "ychubachi/org-sync-gtasks"
    ;;                            :branch "develop")
    :init
    (leaf request-deferred :straight t)
    (leaf ox-qmd :straight t)
    :config
    (setq load-path (cons "~/git/org-sync-qiita" load-path))
    (require 'org-sync-qiita))

  )

(leaf *Test-Bed
  :init
  ;; Do something
  ;; TODO (leaf hydra :straight t)
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("i" text-scale-increase "in")
    ("o" text-scale-decrease "out"))
  ;; TODO (leaf blackout :straight t)
  )

(my/tick-init-time "end")

(provide 'init.el)
;;; init.el ends here
