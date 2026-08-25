(use-package vc-hooks
      :custom
      (vc-follow-symlinks . t)        ; シンボリックリンクの場合、本体を辿る
      (vc-handled-backends . '(Git))) ; Gitのみ使用

(use-package frame :bind ("<f11>" . toggle-frame-maximized))

(use-package recentf
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

(use-package display-fill-column-indicator
  :hook
  (emacs-startup-hook . global-display-fill-column-indicator-mode))

(use-package save-place
  :custom
  (save-place . t)
  :hook
  (emacs-startup-hook . save-place-mode))

(use-package midnight
  :url "https://www.emacswiki.org/emacs/MidnightMode"
  :custom
  ((clean-buffer-list-delay-general . 1))
  :hook
  (emacs-startup-hook . midnight-mode))

(use-package yasnippet-snippets
  :straight t
  :custom
  (yasnippet-snippets-dir . "~/.emacs.d/etc/yasnippet/snippets")
  :hook
  (emacs-startup-hook . yas-global-mode))

(use-package git-gutter
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

(use-package auto-revert
  :custom
  (auto-revert-interval . 1)      ; 再読み込みの間隔
  (auto-revert-verbose . nil)     ; 再読込の際、メッセージを非表示
  (auto-revert-check-vc-info . t) ; VCで更新があった場合、自動で更新
  :init
  (global-auto-revert-mode 1))

(use-package savehist
          ;; Persist history over Emacs restarts.
          ;; Vertico sorts by history position.
          :init
          (savehist-mode 1))

(use-package show-paren-mode
          :custom
          (show-paren-style . 'mixed)
          :init
          (show-paren-mode 1))

(use-package goto-addr
          :doc "Toggle Goto-Address mode in all buffers."
          :url "https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html"
          :init
          ;; You can follow the URL by typing C-c RET
          (global-goto-address-mode 1))

(use-package whitespace
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

(use-package outline-mode
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

(use-package outline-magic
          :init
          (define-key outline-minor-mode-map (kbd "<tab>") 'outline-cycle))


(use-package embark
  :url "https://github.com/oantolin/embark"

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

  (use-package FIXME:my-embark-orglink
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

      (use-package consult
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

(use-package embark-consult
  :doc "Consult users will also want the embark-consult package."

  :after (embark consult)
  ;; :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
)

(use-package yaml-mode )
(use-package popup )
(use-package list-utils )
(use-package iedit )
(use-package files+ )
(use-package ls-lisp+ )
(use-package w32-browser )
(use-package dired+
  :straight (dired+ :type git :host github
                    :repo "emacsmirror/dired-plus"))

(use-package minions

  :require t
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]")
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))

(use-package projectile

  :require t
  :bind ((projectile-mode-map
          ("C-c p" . projectile-command-map))
         (projectile-command-map
          ("b" . consult-project-buffer)))
  :config
  (setq projectile-project-search-path '("~/.emacs.d/" ("~/git" . 1)))
  (projectile-mode 1))

(use-package perspective

  :config
  (global-set-key (kbd "C-x C-b") 'persp-list-buffers)
  (customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-mode))

;; (consult-customize consult--source-buffer :hidden t :default nil)
;; (add-to-list 'consult-buffer-sources persp-consult-source)



(use-package magit

  :bind (("C-x g" . magit-status)))

(use-package swap-buffers

  :bind
  ("C-c b" . swap-buffers)
  :custom
  (swap-buffers-qwerty-shortcuts
   . '("a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-")))

(use-package shell-pop

  :bind
  ("C-c s" . shell-pop)
  :custom
  (shell-pop-shell-type . (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
  (shell-pop-window-position . "bottom")
  (setq shell-pop-full-span . t))

(use-package page-break-lines

  :require t)

(use-package all-the-icons )

(use-package dashboard
  :require t

  :config
  (dashboard-setup-startup-hook))

(use-package *cmd/browser
  :config
  (defun cmd/wsl-browser (url &rest ignore)
    "Browse URL using wslview."
    (interactive "sURL: ")
    (shell-command (concat "wslview " "'" url "'")))

  (when (and (eq system-type 'gnu/linux)
             (getenv "WSLENV"))
    (setq browse-url-browser-function 'cmd/wsl-browser)
    (setq browse-url-generic-program "web-browser")))

(use-package visual-fill-column
  :doc "fill-column for visual-line-mode"
  :req "emacs-25.1"
  :tag "emacs>=25.1"
  :url "https://github.com/joostkremers/visual-fill-column"
  :added "2021-11-08"
  :emacs>= 25.1

  :hook (org-mode-hook . visual-fill-column-mode)
  :bind(("C-c q" . visual-fill-column-mode)
        (:visual-fill-column-mode-map
         ("C-a" . beginning-of-visual-line)
         ("C-e" . end-of-visual-line)
         ("C-k" . kill-visual-line))))

(use-package imenu-list
  :bind (("C-c i" . imenu-list-smart-toggle))
  :hook
  (imenu-list-major-mode-hook . (lambda nil (display-line-numbers-mode -1))))

;; (use-package simple
;;   :bind ("M-SPC" . cycle-spacing)) ; Not working when ALT-SPC is typed.

(use-package ffap
  :doc "ポイント位置のファイルやURLを開く"
  :url "https://ayatakesi.github.io/emacs/25.1/FFAP.html"
  :init
  (ffap-bindings))


(use-package backup-each-save
  :when (not (eq system-type 'windows-nt))

  :custom
  (backup-each-save-mirror-location . "~/.emacs.d/backups") ; バックアップ先
  (backup-each-save-time-format . "%y%m%d_%H%M%S") ; バックアップファイルにつけるsuffix
  (backup-each-save-size-limit . 5000000) ; バックアップするファイルサイズの上限
  (backup-each-save-filter-function . 'identity) ; すべてのファイルをバックアップする
  :init
  (add-hook 'after-save-hook 'backup-each-save)) ; 有効化！


(use-package pandoc-mode
  :doc "Minor mode for interacting with Pandoc"
  :req "hydra-0.10.0" "dash-2.10.0"
  :tag "pandoc" "text"
  :added "2020-11-24"
  :url "http://joostkremers.github.io/pandoc-mode/"

  :after hydra)

(use-package dired-launch

  :hook (dired-mode-hook . dired-launch-mode)
  :config
  (when (and (eq system-type 'gnu/linux)
             (getenv "WSLENV"))
    (setq dired-launch-default-launcher '("wslview"))))

(use-package region-bindings-mode

  :require t
  :config
  (region-bindings-mode-enable))

(use-package multiple-cursors

  :after region-bindings-mode
  :config
  (define-key region-bindings-mode-map "e" 'mc/edit-lines)
  (define-key region-bindings-mode-map "a" 'mc/mark-all-like-this)
  (define-key region-bindings-mode-map "p" 'mc/mark-previous-like-this)
  (define-key region-bindings-mode-map "n" 'mc/mark-next-like-this)
  (define-key region-bindings-mode-map "m" 'mc/mark-more-like-this-extended))

(use-package align
  :doc "align text to a specific column, by regexp"
  :tag "builtin"
  :added "2021-10-30"
  :after region-bindings-mode
  :config
  (define-key region-bindings-mode-map "=" 'align-regexp))

(use-package eww
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

(use-package Disabled
  :disabled t
  :init

  (use-package Line-Numbers-And-Ruler
    :init
    (use-package display-line-numbers
      :custom
      (display-line-numbers-width . 5) ; 表示する行番号の桁数
      :hook
      (emacs-startup-hook . global-display-line-numbers-mode)))


  (use-package blackout )

  (use-package origami
    :url "http://emacs.rubikitch.com/origami/"
    )

  (use-package corfu
    :disabled t ;; TODO
    :doc "Completion Overlay Region FUnction"
    :url "https://github.com/minad/corfu"

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
    (use-package emacs
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

(use-package moody

  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package beacon

  :custom
  (beacon-blink-when-focused . nil)
  :config
  (beacon-mode 1))

(use-package ruler-mode
  :config
  (add-hook 'find-file-hook (lambda () (ruler-mode 1))))

(use-package nerd-icons )

(use-package *auto-indent-yanked-code
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


(use-package coverage )

(use-package dockerfile-mode
  :config
  ;; Set dockerfile-image-name as safe variable.
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

(use-package highlight-indent-guides

  :require t
  :hook
  ((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled . t)
  (highlight-indent-guides-responsive . t)
  (highlight-indent-guides-method . 'column) ; fill character column
  )

(use-package rainbow-delimiters

  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package paredit

  :commands enable-paredit-mode
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
         (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
         (ielm-mode-hook . enable-paredit-mode)
         (lisp-mode-hook . enable-paredit-mode)
         (lisp-interaction-mode-hook . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode)))

(use-package macrostep               ; to test use-package macros.
  :doc "interactive macro expander"
  :url "https://github.com/emacsorphanage/macrostep"

  :require t ; checked
  :bind ((:emacs-lisp-mode-map
          ("C-c e" . macrostep-expand))))

(use-package flycheck
  :doc "On-the-fly syntax checking"
  :emacs>= 24.3

  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom ((flycheck-emacs-lisp-initialize-packages . t)
           (flycheck-disabled-checkers . '(emacs-lisp-checkdoc)))
  :hook (emacs-lisp-mode-hook lisp-interaction-mode-hook)
  :config
  (use-package flycheck-package
    :doc "A Flycheck checker for elisp package authors"

    :config
    (flycheck-package-setup))

  (use-package flycheck-elsa
    :doc "Flycheck for Elsa."
    :emacs>= 25

    :config
    (flycheck-elsa-setup)))

(use-package ert
  ;; TODO: Make it as Emacs-Lisp-Mode binding
  :bind (("C-c t" . cmd/run-ert))
  :config
  (defun cmd/run-ert ()
    (interactive)
    (eval-buffer)
    (call-interactively 'ert)))

(use-package emacs-refactor

  :bind ((emacs-lisp-mode-map
          ("M-RET" . emr-show-refactor-menu))))

(use-package notmuch
      :when (not (eq system-type 'windows-nt))

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


(use-package ol-notmuch

  :require t
  :after notmuch org)

(use-package consult-notmuch
  :when (not (eq system-type 'windows-nt))
  ;; :straight (consult-notmuch :type git :host github
  ;;                            :repo "emacsmirror/consult-notmuch")

  :after consult notmuch)


(use-package mm-decode
  :custom (mm-default-directory . "~/Downloads/"))

(use-package gnus-alias
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


(use-package wanderlust
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

  (define-key wl-summary-mode-map "=" 'my:wl-summary-jump-to-referer-message)

(use-package org2blog

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

(use-package ox-hugo

  :require t
  :after ox)

(use-package ox-zenn

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

(use-package org-publish-project-alist
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


(use-package org-pomodoro

  :require t)


(use-package org-contrib

  :config
  (require 'ox-taskjuggler))

(add-hook 'org-mode-hook
          (lambda () (imenu-add-to-menubar "Imenu")))
(setq org-imenu-depth 3)

(add-hook 'org-mode-hook 'imenu-list-minor-mode)

(use-package org-modern
  :disabled nil
  :url "https://github.com/minad/org-modern"

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


(use-package org-sync-gtasks
  ;; :straight (org-sync-gtasks :type git :host github
  ;;                            :repo "ychubachi/org-sync-gtasks"
  ;;                            :branch "develop")
  :init
  (use-package oauth2 )
  :config
  (setq load-path (cons "~/git/org-sync-gtasks" load-path))
  (require 'org-sync-gtasks))

(use-package org-sync-qiita
  ;; :straight (org-sync-gtasks :type git :host github
  ;;                            :repo "ychubachi/org-sync-gtasks"
  ;;                            :branch "develop")
  :init
  (use-package request-deferred )
  (use-package ox-qmd )
  :config
  (setq load-path (cons "~/git/org-sync-qiita" load-path))
  (require 'org-sync-qiita))

(use-package org-roam
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

(use-package org-roam-ui
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
