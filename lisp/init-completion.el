;;; Completion UI
;;; Vertico - ミニバッファ補完
;;"入力補完の候補をTABを押さずとも一覧から選べるようにする
 ;; https://github.com/minad/vertico

(use-package vertico
  :init
  (vertico-mode))

;;; Orderless - スペース区切りあいまい検索
;; 入力補完の際、複数の語句で検索できるようにする
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil))

;;; Marginalia - 候補に説明を追加
;; 入力補完の候補に説明文を表示する
(use-package marginalia
  :init
  (marginalia-mode))

;;; Consult - 高機能検索・移動コマンド
;; - M-sがconsultの検索のデフォルトプリフィックスと重なるのでC-c sに変更

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
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
         ("C-x t b" . consult-buffer-other-tab) ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map' -> C-c s に変更
         ("C-c s d" . consult-find) ;; Alternative: consult-fd
         ("C-c s c" . consult-locate)
         ("C-c s g" . consult-grep)
         ("C-c s G" . consult-git-grep)
         ("C-c s r" . consult-ripgrep)
         ("C-c s l" . consult-line)
         ("C-c s L" . consult-line-multi)
         ("C-c s k" . consult-keep-lines)
         ("C-c s u" . consult-focus-lines)
         ;; Isearch integration
         ("C-c s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi) ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

;;; Embark - 候補に対するアクション
(use-package embark
  :bind
  (("C-." . embark-act)))

;;; Embark-Consult - EmbarkとConsultの連携
(use-package embark-consult
  :after (embark consult)

  ;; Embark Collect バッファで Consult プレビューを有効化
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Which-Key - キーバインド候補表示
(use-package which-key
  :config
  (which-key-mode))

;;; Corfu - バッファ内でのコード自動補完
(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match 'insert) ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  ;; (corfu-history-mode)
  ;; (corfu-mouse-mode)
  ;; (corfu-popupinfo-mode)
  )

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;;; Cape - Corfu用の補完ソース
;; Abbrev (または abbr.) は、英語の abbreviation（省略、略語、短縮形）の略

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :custom
  (corfu-auto t)                 ; 自動で補完候補をポップアップ
  (corfu-auto-delay 0.0)         ; 遅延なし
  (corfu-auto-prefix 1)          ; 1文字入力で発動
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ; 開いているバッファの単語補完
  (add-hook 'completion-at-point-functions #'cape-file) ; ファイルパス補完
  (add-hook 'completion-at-point-functions #'cape-history) ; ミニバッファ履歴補完
  (add-hook 'completion-at-point-functions #'cape-symbol) ; Emacs Lispシンボル補完
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ; OrgやMarkdown中のElispコード補完
  (add-hook 'completion-at-point-functions #'cape-keyword) ; プログラミング言語の予約語補完
  (add-hook 'completion-at-point-functions #'cape-dict) ; 辞書による英単語補完
  (add-hook 'completion-at-point-functions #'cape-emoji) ; 絵文字補完
  :config
  ;; LaTeX（TeX）モード専用の設定
  (add-hook 'TeX-mode-hook
            (lambda ()
              ;; TeXの数式・コマンド補完を最優先にする
              (add-to-list 'completion-at-point-functions #'cape-tex)
  ;;            記述済みのキーワードをあいまい補完する設定（お好みで）
  ;;            (add-to-list 'completion-at-point-functions #'cape-keyword))
              )
            )
  )

  (provide 'init-completion)
