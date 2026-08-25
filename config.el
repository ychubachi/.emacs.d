;;; config.el --- config.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020,2024 Yoshihide Chubachi

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

;;  My config.el.

;;; Code:

(leaf Help-Keys
        :init
        ;; (define-key key-translation-map [?\C-h] [?\C-?])
        (global-set-key "\C-h" `delete-backward-char)
        (global-set-key (kbd "C-^") help-map))

(leaf Yes-or-no-p
        :init
        (defalias 'yes-or-no-p 'y-or-n-p))

(leaf Coding-System
        :init
        (set-language-environment "Japanese")
        (prefer-coding-system 'utf-8)
        (cond ((eq system-type 'windows-nt)
               (setq default-process-coding-system
                     (cons 'utf-8 'cp932-unix)))))

(leaf mozc
  :straight t
  :defvar (mozc-helper-program-name)
  :init
  (cond
   ((eq system-type 'windows-nt)
    ;; helperのVer 1.13
    (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper-1.13.exe"))
   (t
    ;; helperのVer 2.31
    (setq mozc-helper-program-name "mozc_emacs_helper.sh"))))

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

(leaf mozc-windows
  :if (eq system-type 'windows-nt)
  :defun (mozc-session-sendkey)
  :init
  (advice-add 'mozc-session-execute-command
              :after (lambda (&rest args)
                       (when (eq (nth 0 args) 'CreateSession)
                         (mozc-session-sendkey '(Hankaku/Zenkaku))))))

(leaf isearch
  :bind ((:isearch-mode-map
          ("C-o" . isearch-toggle-input-method))))

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

(leaf Fonts
  :doc "フォント設定"
  :init
  ;; ｜あいうえお｜
  ;; ｜憂鬱な檸檬｜
  ;; ｜<miilwiim>｜
  ;; ｜!"#$%&'~{}｜
  ;; ｜🙆iimmiim>｜
  (custom-set-faces
   '(default ((t (:family "HackGen"))))
   ;; '(default ((t (:family "Noto Sans"))))
   ;; '(default ((t (:family "PlemolJP"))))
   ;; '(default ((t (:family "UDEV Gothic NF"))))
   ;; '(default ((t (:family "FirgeNerd"))))
   ))

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
   ))

(leaf vc-hooks
      :custom
      (vc-follow-symlinks . t)        ; シンボリックリンクの場合、本体を辿る
      (vc-handled-backends . '(Git))) ; Gitのみ使用

(leaf frame :bind ("<f11>" . toggle-frame-maximized))

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
          :config
          (run-at-time nil (* 5 60)
                       (lambda ()
                         (let ((save-silently t)) ; FIXME
                           (recentf-save-list)))))

(leaf display-fill-column-indicator
  :hook
  (emacs-startup-hook . global-display-fill-column-indicator-mode))

(leaf save-place
  :custom
  (save-place . t)
  :hook
  (emacs-startup-hook . save-place-mode))

(leaf midnight
  :url "https://www.emacswiki.org/emacs/MidnightMode"
  :custom
  ((clean-buffer-list-delay-general . 1))
  :hook
  (emacs-startup-hook . midnight-mode))

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
      (emacs-startup-hook . global-git-gutter-mode))

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
  :init
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(leaf wdired
  :doc "Rename files editing their names in dired buffers"
  :tag "builtin"
  :added "2020-11-21"
  :require t
  :config
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
  :bind ((wdired-mode-map
          ("C-o" . toggle-input-method))))

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
)

(leaf Org-Settings
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :custom
  (org-directory . "~/Dropbox/Org/")
  (org-default-notes-file . "~/Dropbox/Org/Notebook.org")
  (org-agenda-files . '("~/Dropbox/Org/"))
  (org-refile-targets . '((org-agenda-files :tag . "REFILE")
                          (nil :tag . "REFILE")))

  (org-todo-keyword-faces
   . '(("NEXT" . (:foreground "blue" :underline t))
       ("DONE" . (:foreground "pale green"))))
  (org-todo-keywords . '((sequence "TODO" "NEXT" "|" "DONE" "SOMEDAY")))

  (org-startup-truncated . nil)
  (org-return-follows-link  . t)          ; RET/C-mでリンクを開く
  (org-agenda-start-with-follow-mode . t) ; アジェンダで関連するorgファイルを開く
  (org-ellipsis . "↴")                   ; ▽,…,▼, ↴, ⬎, ⤷, ⋱
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
  (add-to-list 'org-structure-template-alist
               '("ai" . "ai")))

(leaf *org-use-speed-commands
  :config
  (setq org-use-speed-commands
        (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**")))))

(leaf org-superstar
  :disabled nil
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda nil (org-superstar-mode 1)))
  (setq org-superstar-headline-bullets-list
        '("●" "■" "▷" "○"))
  (setq org-superstar-item-bullet-alist ; (*,+,-)
        ;; '((42 . 8226) (43 . 10148) (45 . 8211)))
        '((42 . 8226) (43 . 10148) (45 . 65517))))

(leaf org-ai
  :straight (org-ai :type git :host github :repo "rksm/org-ai"
                    :local-repo "org-ai"
                    :files ("*.el" "README.md" "snippets"))
  :hook (org-mode-hook . org-ai-mode)
  :init
  (org-ai-global-mode))

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

(leaf ox-pandoc :straight t :require t)

(leaf yaml-mode :straight t)
(leaf popup :straight t)
(leaf list-utils :straight t)
(leaf iedit :straight t)
(leaf files+ :straight t)
(leaf ls-lisp+ :straight t)
(leaf w32-browser :straight t)
(leaf dired+
  :straight (dired+ :type git :host github
                    :repo "emacsmirror/dired-plus"))

(leaf minions
  :straight t
  :require t
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]")
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))

(leaf projectile
  :straight t
  :require t
  :bind ((projectile-mode-map
          ("C-c p" . projectile-command-map))
         (projectile-command-map
          ("b" . consult-project-buffer)))
  :config
  (setq projectile-project-search-path '("~/.emacs.d/" ("~/git" . 1)))
  (projectile-mode 1))

(leaf perspective
  :straight t
  :config
  (global-set-key (kbd "C-x C-b") 'persp-list-buffers)
  (customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-mode))

;; (consult-customize consult--source-buffer :hidden t :default nil)
;; (add-to-list 'consult-buffer-sources persp-consult-source)



(leaf magit
  :straight t
  :bind (("C-x g" . magit-status)))

(leaf swap-buffers
  :straight t
  :bind
  ("C-c b" . swap-buffers)
  :custom
  (swap-buffers-qwerty-shortcuts
   . '("a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-")))

(leaf shell-pop
  :straight t
  :bind
  ("C-c s" . shell-pop)
  :custom
  (shell-pop-shell-type . (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
  (shell-pop-window-position . "bottom")
  (setq shell-pop-full-span . t))

(leaf page-break-lines
  :straight t
  :require t)

(leaf all-the-icons :straight t)

(leaf dashboard
  :require t
  :straight t
  :config
  (dashboard-setup-startup-hook))

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

(leaf org-roam :straight t
  :init
  (setq browse-url-galeon-program nil) ; FIXME
  (setq browse-url-netscape-program nil) ; FIXME
  :custom
  (org-roam-directory . "~/Dropbox/Org/Roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; (message "org-roam :config")
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)[[id:15c4cac2-e75f-4131-96b1-1f9c4ff9a409][ほげ]]
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(leaf org-roam-ui :straight t
  :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide 'config)
;;; config.el ends here
