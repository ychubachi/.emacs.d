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
;; (prog1 "Start Profiling"
;;   (defvar my/tick-previous-time (current-time))

;;   (defun my/tick-init-time (msg)
;;     "Tick boot sequence."
;;     (let ((ctime (current-time)))
;;       (message "--- %5.2f[ms] %s"
;;                (* 1000 (float-time
;;                         (time-subtract ctime my/tick-previous-time)))
;;                msg)
;;       (setq my/tick-previous-time ctime)))

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
    (leaf-keywords-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings with Leaf

(leaf Minimum
  :disabled nil
  :init
  (leaf Hi-Priority-Packages
    :init
    (leaf no-littering
      :url "https://github.com/emacscollective/no-littering#usage"
      :straight t :require t)

    (leaf org
      :straight t))

  (leaf Minimum-Initialization
    :init
    (leaf Help-Keys
      :init
      ;; (define-key key-translation-map [?\C-h] [?\C-?])
      (global-set-key "\C-h" `delete-backward-char)
      (global-set-key (kbd "C-^") help-map))

    (leaf yes-or-no-p
      :init
      (defalias 'yes-or-no-p 'y-or-n-p)))

  (leaf Language-Environment
    :init
    (leaf Coding-System
      :init
      (set-language-environment "Japanese")
      (prefer-coding-system 'utf-8)
      (cond ((eq system-type 'windows-nt)
             (setq default-process-coding-system
                   (cons 'utf-8 'cp932-unix)))))

    (leaf Fonts
      :doc "フォント設定。C-u C-x = で文字毎に確認できる。"
      :init
      ;; ｜あいうえお｜
      ;; ｜憂鬱な檸檬｜
      ;; ｜<miilwiim>｜
      ;; ｜!"#$%&'~{}｜
      ;; ｜🙆iimmiim>｜
      (custom-set-faces
       ;; '(default ((t (:family "Noto Sans"))))
       ;; '(default ((t (:family "PlemolJP"))))
       '(default ((t (:family "HackGen"))))
       ;; '(default ((t (:family "UDEV Gothic NF"))))
       ;; '(default ((t (:family "FirgeNerd"))))
       )))

  (leaf Input-Method
    :init
    (leaf mozc
      :straight t
      :defvar (mozc-helper-program-name)
      :init
      (cond
       ((eq system-type 'windows-nt)
        (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper.exe"))
       (t
        (setq mozc-helper-program-name "mozc_emacs_helper"))))

    (leaf mozc-im
      :straight t
      :require t                        ; Checked
      :custom (default-input-method . "japanese-mozc-im")
      :bind* (("C-o" . toggle-input-method))
      :defvar (mozc-candidate-style)
      :init
      (setq mozc-candidate-style 'echo-area))

    (leaf mozc-cursor-color
      :straight (mozc-cursor-color :type git :host github
                                   :repo "iRi-E/mozc-el-extensions")
      :require t                        ; Checked
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

    (leaf isearch
      :bind ((isearch-mode-map
              ("C-o" . isearch-toggle-input-method))))

    (leaf mozc-windows
      :if (eq system-type 'windows-nt)
      :defun (mozc-session-sendkey)
      :init
      (advice-add 'mozc-session-execute-command
  	          :after (lambda (&rest args)
  	                   (when (eq (nth 0 args) 'CreateSession)
  	                     (mozc-session-sendkey '(Hankaku/Zenkaku))))))
    )
  ;; end of Minimum
  )

(leaf Main
  :disabled nil
  :init
  (leaf Builtin-Packages
    :init
    (leaf Variables
      :init
      (leaf Emacs-Variables
        :custom
        ((inhibit-startup-screen . t)   ; スタートアップスクリーンを非表示
         (ring-bell-function . 'ignore) ; ベルを鳴らさない
         (fill-column . 80)             ; 80桁で改行（モードによる）
         (indent-tabs-mode . nil)       ; インデントの際タブを使わない
         (byte-compile-warnings
          . '(not cl-functions obsolete)) ; (require 'cl)を検査しない
         (epg-pinentry-mode . 'loopback)  ; GnuPGのパスフレーズをミニバッファで
         (plstore-cache-passphrase-for-symmetric-encryption . t)
                                        ; パスフレーズをキャッシュ
         (select-active-regions . 'only) ; リージョン選択時の移動を早くする
         (dired-dwim-target . t)        ; diredでターゲットを他のdiredバッファに
         (line-spacing . 0.25)
         )
        :init
        (customize-set-variable
         'custom-file (no-littering-expand-etc-file-name "custom.el"))
        )

      (leaf Mail-Variables
        :custom
        ((user-full-name . "Yoshihide Chubachi")
         (user-mail-address . "yc@aiit.ac.jp")
         (mail-user-agent quote message-user-agent)
         (message-send-mail-function quote smtpmail-send-it)
         (smtpmail-stream-type quote starttls)
         (smtpmail-smtp-server . "smtp.gmail.com")
         (smtpmail-smtp-service . 587)))

      (leaf vc-hooks
        :custom
        (vc-follow-symlinks . t)        ; シンボリックリンクの場合、本体を辿る
        (vc-handled-backends . '(Git))) ; Gitのみ使用

      (leaf files
        :defun (no-littering-expand-var-file-name)
        :custom
        (backup-directory-alist . '(("." . ".backup~")))
        (delete-old-versions . t)
        (version-control . t)
        (auto-save-file-name-transforms
         . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))

    (leaf Global-Minnor-Mode
      :init
      (leaf auto-revert
        :custom
        (auto-revert-interval . 1)      ; 再読み込みの間隔
        (auto-revert-verbose . nil)     ; 再読込の際、メッセージを非表示
        (auto-revert-check-vc-info . t) ; VCで更新があった場合、自動で更新
        :init
        (global-auto-revert-mode 1))

      (leaf savehist
        ;; Persist history over Emacs restarts.
        ;; Vertico sorts by history position.
        :init
        (savehist-mode 1))

      (leaf show-paren-mode
        :custom
        (show-paren-style . 'mixed)
        :init
        (show-paren-mode 1))

      (leaf goto-addr
        :doc "Toggle Goto-Address mode in all buffers."
        :url "https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html"
        :init
        ;; You can follow the URL by typing C-c RET
        (global-goto-address-mode 1))

      (leaf whitespace
        :require 't
        :config
        (setq whitespace-style
              '(
                face                  ; faceで可視化
                trailing              ; 行末
                tabs                  ; タブ
                spaces                ; スペース
                space-mark            ; 表示のマッピング
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

      (leaf outline-mode
        :defvar (outline-mode-prefix-map)
        :custom
        :init
        (require 'outline)
        (eval-after-load "outline"
          '(require 'foldout))
        (add-hook 'outline-minor-mode-hook
	          (lambda () (local-set-key
                              "\C-c\C-o"
	                      outline-mode-prefix-map)))
        ;; (setq outline-regexp ";;;\\(;* [^ \\t\\n]\\|###autoload\\)\\|(\\|  (") ; "  ("を追加
        ;; (outline-minor-mode 1) ; TODO: outline-mode is not GLOBAL minnor mode
        )

      (leaf outline-magic :straight t
	:init
	(define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle)))

    (leaf Global-Key-Bindings
      :init
      (leaf frame
        :bind ("<f11>" . toggle-frame-maximized))

      (leaf imenu-list
        :bind (("C-c i" . imenu-list-smart-toggle))
        :hook
        (imenu-list-major-mode-hook . (lambda nil (display-line-numbers-mode -1))))

      (leaf simple
        :bind ("M-SPC" . cycle-spacing)) ; Not working when ALT-SPC is typed.
      ;; End of Builtin-Packages/Global-Key-Bindings
      )

    (leaf Emacs-Startup-Hook
      :init
      (leaf display-fill-column-indicator
        :hook
        (emacs-startup-hook . global-display-fill-column-indicator-mode))

      (leaf save-place
        :custom
        (save-place . t)
        :hook
        (emacs-startup-hook . save-place-mode))

      (leaf recentf
        :custom
        (recentf-max-menu-items  . 500)
        (recentf-max-saved-items . 2000)
        (recentf-auto-cleanup    . 'never)
        (recentf-exclude . '("/recentf" "COMMIT_EDITMSG" "/.?TAGS"
                             "^/sudo:" "/straight"))
        :hook
        (emacs-startup-hook . recentf-mode)
        :defun (recentf-save-list)
        :defvar (recentf-exclude)
        :defvar (no-littering-var-directory no-littering-etc-directory)
        :config
        (run-at-time nil (* 5 60)
                     (lambda ()
                       (let ((save-silently t)) ; FIXME
                         (recentf-save-list))))

        (prog1 "no-littering"
          (add-to-list 'recentf-exclude no-littering-var-directory)
          (add-to-list 'recentf-exclude no-littering-etc-directory)))

      (leaf midnight
	:url "https://www.emacswiki.org/emacs/MidnightMode"
        :custom
        ((clean-buffer-list-delay-general . 1))
        :hook
        (emacs-startup-hook . midnight-mode)))

    (leaf Before-Save-Hook
      :init
      (leaf delete-trailing-whitespace
        :init
        (add-hook 'before-save-hook 'delete-trailing-whitespace))
      )

    (leaf Minnor-Mode-Settings
      :init
      (leaf wdired
        :doc "Rename files editing their names in dired buffers"
        :tag "builtin"
        :added "2020-11-21"
        :require t
        :config
        (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
        :bind ((wdired-mode-map
                ("C-o" . toggle-input-method))))
      )

    (leaf Advices
      :init
      (leaf tramp
        :config
        (defadvice tramp-sh-handle-vc-registered (around tramp-sh-handle-vc-registered activate)
          (let ((vc-handled-backends nil)) ad-do-it)))))

  (leaf External-Packages
    :init
    (leaf Leaf-Extentions
      :init
      (leaf leaf-tree
        :straight t
        :custom (imenu-list-position . 'left)
        :defun (leaf-tree-mode)
        :init
        (defun my/enable-init-el-minor-mode ()
          (when (equal
                 (buffer-file-name)
                 (expand-file-name "~/.emacs.d/init.el"))
            (leaf-tree-mode t)))
        (add-hook 'find-file-hook 'my/enable-init-el-minor-mode))

      (leaf leaf-convert :straight t))

    (leaf Install-Only-Packages
      :init
      (leaf yaml-mode :straight t)
      (leaf popup :straight t)
      (leaf list-utils :straight t)
      (leaf iedit :straight t)
      (leaf files+ :straight t)
      (leaf ls-lisp+ :straight t)
      (leaf w32-browser :straight t)
      (leaf dired+
        :straight (dired+ :type git :host github
                          :repo "emacsmirror/dired-plus")))

    (leaf KeyboardUI
      :init
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


      (leaf hydra :straight t
	:init
	(defhydra hydra-zoom (global-map "<f12>")
	  "zoom"
	  ("i" text-scale-increase "Zoom in")
	  ("o" text-scale-decrease "Zoom out")
	  ("l" global-display-line-numbers-mode "Line number"))

	(defhydra hydra-buffer-menu (:color pink
                                            :hint nil)
	  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
	  ("m" Buffer-menu-mark)
	  ("u" Buffer-menu-unmark)
	  ("U" Buffer-menu-backup-unmark)
	  ("d" Buffer-menu-delete)
	  ("D" Buffer-menu-delete-backwards)
	  ("s" Buffer-menu-save)
	  ("~" Buffer-menu-not-modified)
	  ("x" Buffer-menu-execute)
	  ("b" Buffer-menu-bury)
	  ("g" revert-buffer)
	  ("T" Buffer-menu-toggle-files-only)
	  ("O" Buffer-menu-multi-occur :color blue)
	  ("I" Buffer-menu-isearch-buffers :color blue)
	  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
	  ("c" nil "cancel")
	  ("v" Buffer-menu-select "select" :color blue)
	  ("o" Buffer-menu-other-window "other-window" :color blue)
	  ("q" quit-window "quit" :color blue))

	(define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body))
      )

    (leaf CompletionUI
      :init
      (leaf vertico
        :doc "入力補完の候補をTABを押さずとも一覧から選べるようにする"
        :url "https://github.com/minad/vertico"
        :straight t
        :custom
        (vertico-count . 20)            ; 最大20件まで表示するように
        :config
        (vertico-mode)
        (setq vertico-resize t)
        (setq vertico-cycle t))

      (leaf orderless
        :doc "入力補完の際、複数の語句で検索できるようにする"
        :straight t
        :init
        ;; Configure a custom style dispatcher (see the Consult wiki)
        ;; (setq orderless-style-dispatchers '(+orderless-dispatch))
        (setq completion-styles '(orderless basic)
              completion-category-defaults nil
              completion-category-overrides '((file (styles partial-completion)))))

      (leaf marginalia
        :doc "入力補完の候補に説明文を表示する"
        :straight t
        :bind (:minibuffer-local-map
               ("M-A" . marginalia-cycle))
        :init
        (marginalia-mode))

      (leaf embark
        :url "https://github.com/oantolin/embark"
        :straight t
        :bind
        (("M-." . embark-act)        ; アクションの一覧を表示
         ("C-." . embark-dwim)       ; Do What I Mean デフォルトアクションを実行
         ("C-^ B" . embark-bindings) ; C-h -> C-^ に
         )
        :init
        (setq prefix-help-command #'embark-prefix-help-command)
        :config
        (add-to-list 'display-buffer-alist
                     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                       nil
                       (window-parameters (mode-line-format . none))))

        (leaf FIXME:my-embark-orglink
          :disabled t                   ; FIXME: embark-define-keymapは古い
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

      (leaf consult
        :url "https://github.com/minad/consult"
        :doc "Example configuration for Consult"
        :straight (consult :type git :host github
                           :repo "minad/consult")
        :bind (;; C-c bindings in `mode-specific-map'
               ("C-c M-x" . consult-mode-command)
               ("C-c h" . consult-history)
               ("C-c k" . consult-kmacro)
               ("C-c m" . consult-man)
               ("C-c i" . consult-info)
               ([remap Info-search] . consult-info)
               ;; C-x bindings in `ctl-x-map'
               ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
               ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
               ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
               ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
               ("C-x r b" . consult-bookmark)           ;; orig. bookmark-jump
               ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
               ;; Custom M-# bindings for fast register access
               ("M-#" . consult-register-load)
               ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
               ("C-M-#" . consult-register)
               ;; Other custom bindings
               ("M-y" . consult-yank-pop) ;; orig. yank-pop
               ;; M-g bindings in `goto-map'
               ("M-g e" . consult-compile-error)
               ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
               ("M-g g" . consult-goto-line)   ;; orig. goto-line
               ("M-g M-g" . consult-goto-line) ;; orig. goto-line
               ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
               ("M-g m" . consult-mark)
               ("M-g k" . consult-global-mark)
               ("M-g i" . consult-imenu)
               ("M-g I" . consult-imenu-multi)
               ;; M-s bindings in `search-map'
               ("M-s d" . consult-find)
               ("M-s D" . consult-locate)
               ("M-s g" . consult-grep)
               ("M-s G" . consult-git-grep)
               ("M-s r" . consult-ripgrep)
               ("M-s l" . consult-line)
               ("M-s L" . consult-line-multi)
               ("M-s k" . consult-keep-lines)
               ("M-s u" . consult-focus-lines)
               ;; Isearch integration
               ("M-s e" . consult-isearch-history)
               (:isearch-mode-map
                ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
                ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
                ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
                ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
                )
               ;; Minibuffer history
               (:minibuffer-local-map
                ("M-s" . consult-history) ;; orig. next-matching-history-element
                ("M-r" . consult-history)) ;; orig. previous-matching-history-element
               )

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
         consult-theme :preview-key '(:debounce 0.2 any)
         consult-ripgrep consult-git-grep consult-grep
         consult-bookmark consult-recent-file consult-xref
         consult--source-bookmark consult--source-recent-file
         consult--source-project-recent-file
         :preview-key '(:debounce 0.4 any))

        ;; Optionally configure the narrowing key.
        ;; Both < and C-+ work reasonably well.
        (setq consult-narrow-key "<") ;; (kbd "C-+")

        ;; Optionally make narrowing help available in the minibuffer.
        ;; You may want to use `embark-prefix-help-command' or which-key instead.
        ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

        ;; By default `consult-project-function' uses `project-root' from project.el.
        ;; Optionally configure a different project root function.
;;;; 1. project.el (the default) -> Error
        ;; (setq consult-project-function #'consult--default-project--function)
;;;; 2. vc.el (vc-root-dir)
        ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
;;;; 3. locate-dominating-file
        ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
;;;; 4. projectile.el (projectile-project-root)
        ;; (autoload 'projectile-project-root "projectile")
        ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
;;;; 5. No project support
        ;; (setq consult-project-function nil)

        (leaf embark-consult
          :doc "Consult users will also want the embark-consult package."
          :straight t
          :after (embark consult)
          ;; :demand t ; only necessary if you have the hook below
          ;; if you want to have consult previews as you move around an
          ;; auto-updating embark collect buffer
          :hook
          (embark-collect-mode . consult-preview-at-point-mode))
        ))

    (leaf Look-And-Feel
      :init
      (leaf modus-themes
        :disabled t
        :straight t                     ; omit this to use the built-in themes
        :custom
        (modus-themes-italic-constructs . nil)
        (modus-themes-bold-constructs . nil)
        (modus-themes-region . '(bg-only no-extend))
        (modus-themes-org-blocks . 'gray-background) ; {nil,'gray-background,'tinted-background}
        (modus-themes-subtle-line-numbers . t)
        (modus-themes-mode-line . '(moody borderless (padding . 0) (height . 0.9)))
        (modus-themes-syntax . '(yellow-comments green-strings))
        (modus-themes-hl-line . '(underline accented)) ;'(underline accented)
        (modus-themes-paren-match . '(intense underline))
        ;; (modus-themes-headings . ; this is an alist: read the manual or its doc string
        ;;                        ;; https://typescale.com/ 1.125 - Major Second
        ;;                        '((1 . (bold 1.802))
        ;;                          (2 . (regular 1.602))
        ;;                          (3 . (bold 1.424))
        ;;                          (4 . (regular 1.266))
        ;;                          (5 . (reqular 1.125))
        ;;                          (t . (regular))))
        :init
        (require-theme 'modus-themes)
        ;; Load the theme of your choice:
        (load-theme 'modus-operandi :no-confirm)
        ;; (load-theme 'modus-vivendi :no-confirm)
        :bind
        ("<f5>" . modus-themes-toggle))

      (leaf moody
        :straight t
        :defun (moody-replace-mode-line-buffer-identification
                moody-replace-vc-mode
                moody-replace-eldoc-minibuffer-message-function)
        :config
        (setq x-underline-at-descent-line t)
        (moody-replace-mode-line-buffer-identification)
        (moody-replace-vc-mode)
        (moody-replace-eldoc-minibuffer-message-function))

      (leaf minions
        :doc "A minor-mode menu for the mode line"
        :url "https://github.com/tarsius/minions"
        :straight t
        :custom (minions-mode-line-lighter . "[+]")
        :defun (minions-mode)
        :config
        (minions-mode 1)
        (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))

      (leaf beacon
        :straight t
        :custom
        (beacon-blink-when-focused . t)
        :config
        (beacon-mode 1))

      (leaf all-the-icons :straight t))

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
        (org-refile-targets . '((org-agenda-files :tag . "REFILE")))

        (org-todo-keyword-faces
         . '(("NEXT" . (:foreground "blue" :underline t))
             ("DONE" . (:foreground "pale green"))))
        (org-todo-keywords . '((sequence "TODO" "NEXT" "|" "DONE" "SOMEDAY")))

        (org-startup-truncated . nil)
        (org-return-follows-link  . t)          ; RET/C-mでリンクを開く
        (org-agenda-start-with-follow-mode . t) ; アジェンダで関連するorgファイルを開く
        (org-ellipsis . "↴")                  ; ▽,…,▼, ↴, ⬎, ⤷, ⋱
        (org-export-with-sub-superscripts . nil) ; A^x B_z のような添字の処理をしない
        ;; (org-agenda-remove-tags . t)             ; アジェンダにタグを表示しない
        (org-id-link-to-org-use-id . 'create-if-interactive-and-no-custom-id)
        )

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

      (leaf Org-Look-And-Feel
        :init
        (leaf org-modern
          :disabled nil
          :url "https://github.com/minad/org-modern"
          :straight t
          :custom
          ;;  dashが全角で表示されるので修正
          ((org-modern-list . '((?+ . "◦") (?- . "-") (?* . "•")))
           (org-modern-star . '("■"
                                ".◆"
                                "..●"
                                "...＊"
                                "....＋"))) ; ■
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
          (global-org-modern-mode))

        (leaf org-superstar
          :disabled t
          :straight t
          :config
          (add-hook 'org-mode-hook (lambda nil (org-superstar-mode 1)))
          (setq org-superstar-headline-bullets-list
                '("●" "■" "▷" "○"))) ; TODO: org-modernと重複？
        )

      (leaf Org-Documentation
        :init
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
                       "\\usepackage{minted}" t))

        (leaf ox-beamer
          :require t
          :custom
          (org-beamer-outline-frame-title . "目次")
          (org-beamer-frame-default-options . "t"))

        (leaf ox-pandoc :straight t :require t))

      (leaf Org-Publishing
        :init
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
          (setq org2blog/wp-use-sourcecode-shortcode t))

        (leaf ox-hugo
          :straight t
          :require t
          :after ox)

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

        (leaf org-publish-project-alist
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
                   :recursive t)))))

      (leaf Org-Editing
        :init
        (leaf *org-use-speed-commands
          :config
          (setq org-use-speed-commands
                (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**")))))
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
                       '("f" . "figure")))
        )

      (leaf Org-Other
        :init
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
        (leaf org-pomodoro
          :straight t
          :require t)

        (leaf org-contrib
          :straight t
          :config
          (require 'ox-taskjuggler))

        (leaf *org-plantuml
          :init
          ;; plantuml.jarへのパスを設定
          (setq org-plantuml-jar-path "~/.emacs.d/lib/plantuml-1.2022.12.jar")

          ;; org-babelで使用する言語を登録
          (org-babel-do-load-languages
           'org-babel-load-languages
           '((plantuml . t)))
          )
        (leaf org-download
          :straight t
          :require t
          :custom (org-download-method . 'attach)
          :config
          (setq org-image-actual-width 400) ; width of images (#+ATTR_ORG: 400)
          (add-hook 'dired-mode-hook 'org-download-enable)
          (leaf
            :when (eq system-type 'windows-nt)
            :custom (org-download-screenshot-method . "magick convert clipboard: %s")))
        )

)

    (leaf Mail-Client
      :init
      (leaf notmuch
        :when (not (eq system-type 'windows-nt))
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
        :when (not (eq system-type 'windows-nt))
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
         wl-fcc-force-as-read t  ; Mark sent mail (in the wl-fcc folder) as read
         wl-default-spec "%")    ; For auto-completion

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
        (define-key wl-summary-mode-map "=" 'my:wl-summary-jump-to-referer-message)))

    (leaf Development
      :init
      (leaf *auto-indent-yanked-code
        :url "https://www.emacswiki.org/emacs/AutoIndentation"
        :init
        (dolist (command '(yank yank-pop))
          (eval `(defadvice ,command (after indent-region activate)
                   (and (not current-prefix-arg)
                        (member major-mode '(emacs-lisp-mode lisp-mode
                                                             clojure-mode    scheme-mode
                                                             haskell-mode    ruby-mode
                                                             rspec-mode      python-mode
                                                             c-mode          c++-mode
                                                             objc-mode       latex-mode
                                                             plain-tex-mode))
                        (let ((mark-even-if-inactive transient-mark-mode))
                          (indent-region (region-beginning) (region-end) nil)))))))

      (leaf coverage :straight t)

      (leaf dockerfile-mode :straight t
        :config
        ;; Set dockerfile-image-name as safe variable.
        (put 'dockerfile-image-name 'safe-local-variable #'stringp))

      (leaf Emacs-Lisp
        :init
        (leaf paredit
          :straight t
          :commands enable-paredit-mode
          :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
                 (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
                 (ielm-mode-hook . enable-paredit-mode)
                 (lisp-mode-hook . enable-paredit-mode)
                 (lisp-interaction-mode-hook . enable-paredit-mode)
                 (scheme-mode-hook . enable-paredit-mode)))

        (leaf Global-Bindings
          :init
          (leaf macrostep               ; to test leaf macros.
            :doc "interactive macro expander"
            :url "https://github.com/joddie/macrostep"
            :straight t
            :bind (("C-c e" . macrostep-expand)))) ;; TODO: elisp mode?

        (leaf Emacs-Lisp-Mode-Hook
          :init
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
              (flycheck-elsa-setup)))

          (leaf ert
            ;; TODO: Make it as Emacs-Lisp-Mode binding
            :bind (("C-c t" . cmd/run-ert))
            :config
            (defun cmd/run-ert ()
              (interactive)
              (eval-buffer)
              (call-interactively 'ert))))

        (leaf Emacs-Lisp-Mode-Map
          :init
          (leaf emacs-refactor
            :straight t
            :bind ((emacs-lisp-mode-map
                    ("M-RET" . emr-show-refactor-menu)))))))

    (leaf Global-Key-Bindings
      :init
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

        (leaf consult                   ; TODO consult?
          :straight t
          :require t
          :config
          (consult-customize consult--source-buffer :hidden t :default nil)
          (add-to-list 'consult-buffer-sources persp-consult-source)))

      (leaf swap-buffers
        :straight t
        :bind
        ("C-c b" . swap-buffers)
        :custom
        (swap-buffers-qwerty-shortcuts
         . '("a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-")))

      (leaf magit
        :doc "A Git porcelain inside Emacs."
        :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
        :tag "vc" "tools" "git" "emacs>=25.1"
        :added "2020-11-30"
        :emacs>= 25.1
        ;; :straight t
        :straight t
        :after git-commit with-editor
        :bind (("C-x g" . magit-status)))

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
      ;; End of Global-Key-Bindings
      )

    (leaf Global-Minnor-Mode
      :init
      (leaf undo-tree
        :doc "https://elpa.gnu.org/packages/undo-tree.html"
        :straight t
        :require t                          ; Checked
        :bind ("C-z" . undo-tree-undo)
        :custom
        (undo-tree-auto-save-history . t)
        (undo-tree-visualizer-diff . t)
        :init
        ;; (defadvice undo-tree-make-history-save-file-name
        ;;     (after undo-tree activate)
        ;;   (setq ad-return-value (concat ad-return-value ".gz")))
        (global-undo-tree-mode))

      (leaf google-this
        :straight t
        :config
        (google-this-mode 1))
      )

    (leaf Emacs-Startup-Hook
      :init
      (leaf yasnippet-snippets
        :straight t
        :custom
        (yasnippet-snippets-dir . "~/.emacs.d/etc/yasnippet/snippets")
        :hook
        (emacs-startup-hook . yas-global-mode))

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
        (emacs-startup-hook . global-git-gutter-mode)))

    (leaf TODO:Unorganized
      :init
      (leaf Misc
        :init
        (leaf *warnings
          :custom
          (warning-suppress-types . '(((yasnippet backquote-change))
                                      (org-element-cache)))))

      (leaf TODO:Move-To-Builtin
        :init
        (leaf ffap
          :doc "ポイント位置のファイルやURLを開く"
          :url "https://ayatakesi.github.io/emacs/25.1/FFAP.html"
          :init
          (ffap-bindings))

        (leaf *cmd/browser
          :config
          (defun cmd/wsl-browser (url &rest ignore)
            "Browse URL using wslview."
            (interactive "sURL: ")
            (shell-command (concat "wslview " "'" url "'")))

          (when (and (eq system-type 'gnu/linux)
                     (getenv "WSLENV"))
            (setq browse-url-browser-function 'cmd/wsl-browser)
            (setq browse-url-generic-program "web-browser"))))

      (leaf backup-each-save
        :when (not (eq system-type 'windows-nt))
        :straight t
        :custom
        (backup-each-save-mirror-location . "~/.emacs.d/backups") ; バックアップ先
        (backup-each-save-time-format . "%y%m%d_%H%M%S") ; バックアップファイルにつけるsuffix
        (backup-each-save-size-limit . 5000000) ; バックアップするファイルサイズの上限
        (backup-each-save-filter-function . 'identity) ; すべてのファイルをバックアップする
        :init
        (add-hook 'after-save-hook 'backup-each-save)) ; 有効化！

      (leaf pandoc-mode
        :doc "Minor mode for interacting with Pandoc"
        :req "hydra-0.10.0" "dash-2.10.0"
        :tag "pandoc" "text"
        :added "2020-11-24"
        :url "http://joostkremers.github.io/pandoc-mode/"
        :straight t
        :after hydra)

      (leaf migemo
        :when (eq system-type 'gnu/linux)
        :straight t
        :require t                      ; Checked on 2023-02-12
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

      (leaf dired-launch
        :straight t
        :hook (dired-mode-hook . dired-launch-mode)
        :config
        (when (and (eq system-type 'gnu/linux)
                   (getenv "WSLENV"))
          (setq dired-launch-default-launcher '("wslview"))))

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

      (leaf Prog-Mode-Hook
        :init
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
          (prog-mode-hook . rainbow-delimiters-mode)))

      (leaf After-Init-Hook
        :init
        )
      )
    )

  (leaf Original-Packages
    :init
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
      (require 'org-sync-qiita))))

(leaf Disabled
  :disabled t
  :init

  (leaf Line-Numbers-And-Ruler
    :init
    (leaf display-line-numbers
      :custom
      (display-line-numbers-width . 5) ; 表示する行番号の桁数
      :hook
      (emacs-startup-hook . global-display-line-numbers-mode))

    (leaf ruler-mode
      :hook
      (find-file-hook . (lambda () (ruler-mode 1)))))

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

  (leaf blackout :straight t)

  (leaf origami
    :url "http://emacs.rubikitch.com/origami/"
    :straight t)

  (leaf corfu
    :disabled t ;; TODO
    :doc "Completion Overlay Region FUnction"
    :url "https://github.com/minad/corfu"
    :straight t
    ;; Optional customizations
    ;; :custom
    ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    ;; (corfu-auto t)                 ;; Enable auto completion
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; Enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
    ;; be used globally (M-/).  See also the customization variable
    ;; `global-corfu-modes' to exclude certain modes.
    :init
    (global-corfu-mode)

    ;; A few more useful configurations...
    (leaf emacs
      :init
      ;; TAB cycle if there are only few candidates
      (setq completion-cycle-threshold 3)

      ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
      ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
      ;;
      (setq read-extended-command-predicate
            #'command-completion-default-include-p)

      ;; Enable indentation+completion using the TAB key.
      ;; `completion-at-point' is often bound to M-TAB.
      (setq tab-always-indent 'complete)))
  )

(leaf Test-Bed
  :init
  ;; Experimental Settings
)

;;   (my/tick-init-time "start"))

;; (my/tick-init-time "end")

(provide 'init.el)
;;; init.el ends here
