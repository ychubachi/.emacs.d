;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2026 Yoshihide Chubachi

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

;; 以前はlisp/init-*.elに分割していたが、1ファイルに統合した。
;; outli-mode（;;; / ;;;; / ;;;;; ... の見出し）で折りたたみ・移動ができる。
;; 見出し一覧（C-c C-n / C-c C-p で移動）:
;;   - パッケージ管理（elpaca）
;;   - Emacs本体の基本設定
;;   - 日本語環境の設定
;;   - ファイル操作の設定
;;   - 見た目の設定
;;   - 補完機能の設定
;;   - Orgモードの設定
;;   - 文書作成・エクスポートの設定
;;   - テキスト編集・ウィンドウ操作の設定
;;   - バージョン管理の設定
;;   - プログラミング支援の設定
;;   - AI関連の設定
;;   - その他の設定

;;; Code:

;;; パッケージ管理（elpaca）
;; elpacaの導入と、他の設定より先に読み込む必要があるパッケージ。

;;;; elpaca
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(when (eq system-type 'windows-nt)
  (elpaca-no-symlink-mode))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;;;; no-littering - Emacsのバックアップファイルや一時ファイルを整理する
(use-package no-littering
  :ensure (:wait t)
  :demand t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))
  ;; Theme standard backups and undo-tree history locations
  (no-littering-theme-backups))

;;;; org - Orgモードの最新版を利用する
(use-package org
  :ensure (:wait t)  ;; Block until the updated Org package is ready
  )

;;; Emacs本体の基本設定
;;;; Emacsの組み込み機能を初期化する
(use-package emacs
  :ensure nil
  :custom
  ;; startup
  (inhibit-startup-screen t)

  ;; ui
  (ring-bell-function #'ignore)
  (line-spacing 0.25)

  ;; editing
  (fill-column 80)
  (indent-tabs-mode nil)
  (select-active-regions 'only)

  ;; byte-compile
  (byte-compile-warnings '(not cl-functions obsolete))

  ;; warnings
  (warning-suppress-types '((yasnippet backquote-change) (org-element-cache)))

  ;; GnuPG
  (epg-pinentry-mode 'loopback)
  (plstore-cache-passphrase-for-symmetric-encryption t)

  ;; user
  (user-full-name "Yoshihide CHUBACHI")
  (user-mail-address "yoshi@chubachi.net")

  :bind
  ("M-SPC" . cycle-spacing)

  :hook
  (before-save . delete-trailing-whitespace)

  :init
  ;; (keyboard-translate ?\C-h ?\C-?)
  (global-set-key (kbd "C-h") #'delete-backward-char) ; C-hをBSにする
  (global-set-key (kbd "C-^") help-map) ; C-hの代わりにC-^をヘルプマップにする

  (defalias 'yes-or-no-p 'y-or-n-p) ; yos/noをy/nに変更する

  (ffap-bindings) ; ffap（ポイント位置のファイルを探す）を有効にする
  (global-goto-address-mode 1) ; バッファ内のすべてのURLやメールアドレスを自動でリンク化（クリック可能に）
  )

;;; 日本語環境の設定
;; 言語環境・文字コードと日本語入力（mozc）の設定。

;;;; 言語環境と文字コードを設定する
(use-package emacs
  :ensure nil
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (cond ((eq system-type 'windows-nt)
	 (setq default-process-coding-system
	       (cons 'utf-8 'cp932-unix)))))

;;;; mozc - 日本語変換用ヘルパーの呼び出し設定
;; WSL(Ubuntu) から利用する場合:
;; - mozc_emacs_helper.sh を作成し、Windows側のexeを呼び出す
(use-package mozc
  :demand t
  :config
  (cond
   ((eq system-type 'windows-nt)
    (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper-2.31.exe"))
   (t
    ;; helperのVer 2.31
    (setq mozc-helper-program-name "mozc_emacs_helper.sh"))))

;;;; mozc-im - インプット方式の設定
(use-package mozc-im
  :after mozc
  :demand t
  :bind
  (("C-o" . toggle-input-method))
  :init
  (setq default-input-method "japanese-mozc-im")

  ;; モードによってカーソルの色を変える
  (defvar my/cursor-color-japanese "cyan")
  (defvar my/cursor-color-default (frame-parameter nil 'cursor-color))
  (add-hook 'input-method-activate-hook
            (lambda () (set-cursor-color my/cursor-color-japanese)))
  (add-hook 'input-method-deactivate-hook
            (lambda () (set-cursor-color my/cursor-color-default)))
  )

;;; ファイル操作の設定 - ファイルの履歴・自動保存・再読込、dired等の設定。

;;;; recentf
(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)

  :custom
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 1000)
  (recentf-auto-cleanup 'never)
  (recentf-exclude '("/recentf" "COMMIT_EDITMSG" "/.?TAGS" "^/sudo:" "/elpaca"))

  :config
  (run-at-time nil (* 5 60)
               #'recentf-save-list))

;;;; savehist
(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1))

;;;; saveplace
(use-package saveplace
  :ensure nil
  :init
  (save-place-mode 1))

;;;; auto-revert
(use-package autorevert
  :ensure nil

  :custom
  (auto-revert-interval 1)
  (auto-revert-verbose nil)
  (auto-revert-check-vc-info t) ; VCで更新があった場合、自動で更新

  :init
  (global-auto-revert-mode 1))

;;;; files
(use-package files
  :ensure nil

  :custom
  (make-backup-files nil)
  (auto-save-default nil)
  (create-lockfiles nil)

  ;; シンボリックリンクを自動で辿る
  (vc-follow-symlinks t))

;;;; dired
(use-package dired
  :ensure nil

  :custom
  (dired-dwim-target t))

;;;; wdired
(use-package wdired
  :ensure nil

  :bind
  (:map dired-mode-map
        ("r" . wdired-change-to-wdired-mode)))

;;;; WSLVIEWはサポート終了？
;; ;;; WSL環境でリンクをクリックした時に、Windows側のブラウザで開く設定
;; ;; wslview (wsluパッケージ) が必要: sudo apt install wslu
;; (use-package emacs
;;   :ensure nil
;;   :config
;;   (defun cmd/wsl-browser (url &rest _ignore)
;;     "Browse URL using wslview."
;;     (interactive "sURL: ")
;;     (shell-command (concat "wslview " "'" url "'")))

;;   (when (and (eq system-type 'gnu/linux)
;;              (getenv "WSLENV"))
;;     (setq browse-url-browser-function 'cmd/wsl-browser)))

;; ;;; Dired上で `J` を押すと、Windows側の既定のアプリ（Word、PDF、画像など）で開く
;; ;; wslview (wsluパッケージ) が必要: sudo apt install wslu
;; (use-package dired-launch
;;   :ensure t
;;   :hook (dired-mode . dired-launch-mode)
;;   :config
;;   (when (and (eq system-type 'gnu/linux)
;;              (getenv "WSLENV"))
;;     (setq dired-launch-default-launcher '("wslview"))))

;;; 見た目の設定
;; テーマ・フォント・フレーム・モードラインなど見た目の設定。

;;;; テーマの設定
(load-theme 'misterioso)

;;;; フォントを設定する
;; Ubuntuの場合、~/.fontsに必要なフォントを入れて
;; # fc-cache -fv
;; を実行

;; ｜あいうえお｜
;; ｜憂鬱な檸檬｜
;; ｜<miilwiim>｜
;; ｜!"#$%&'~{}｜
;; ｜🙆iimmiim>｜
(use-package emacs
  :ensure nil
  :config
  (custom-set-faces
   '(default ((t (:family "HackGen")))) ;; (x-list-fonts "HackGen") で確認可能
   ))

;;;; ウィンドウの余白と境界線
(modify-all-frames-parameters
 '((right-divider-width . 10)
   (internal-border-width . 10)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;;;; frame - 画面の最大化をトグル
(use-package frame
  :ensure nil
  :bind ("<f11>" . toggle-frame-maximized))

;;;; minions - マイナーモード表示をコンパクトにする
(use-package minions
  :ensure t
  :config
  (minions-mode 1)
  (setq minions-mode-line-lighter "[+]")
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))

;;;; beacon - バッファ・ウィンドウ切り替え時にカーソル位置を点滅表示
(use-package beacon
  :ensure t
  :custom
  (beacon-blink-when-focused nil)
  :config
  (beacon-mode 1))

;;;; hydra-zoom - 文字サイズ・行番号表示の切り替え（<f12>）
;; hydra本体の導入は「テキスト編集・ウィンドウ操作の設定」節
(with-eval-after-load 'hydra
  (defhydra hydra-zoom (global-map "<f12>")
    "zoom"
    ("i" text-scale-increase "Zoom in")
    ("o" text-scale-decrease "Zoom out")
    ("l" global-display-line-numbers-mode "Line number")))

;; ;;; dashboard - Emacs起動時にダッシュボードを表示する
;; ;; 起動が遅い
;; (use-package dashboard
;;   :ensure t ; 必要に応じて
;;   :config
;;   ;; 1. 起動時にダッシュボードを表示
;;   (dashboard-setup-startup-hook)

;;   ;; 2. 最も重いアジェンダの読み込みを非同期化する（Emacs 29+ / dashboardの比較的新しいバージョンで有効）
;;   (setq dashboard-agenda-release-buffers t)
;;   (setq dashboard-async-services '(agenda)) ; agendaを非同期処理にする

;;   ;; 3. アイコン描画がボトルネックの場合は、明示的に無効化する
;;   (setq dashboard-set-heading-icons nil)
;;   (setq dashboard-set-file-icons nil)

;;   ;; 4. ダッシュボードに表示する項目
;;   ;; ダッシュボードに表示する項目とその件数
;;   (setq dashboard-items '((recents  . 5)   ; 最近開いたファイル
;;                           (bookmarks . 5)  ; ブックマーク
;;                           (projects . 5)   ; 最近のプロジェクト（Project.elやProjectile連携）
;;                           (agenda . 5))))   ; Org-modeのアジェンダ（予定）

;;; 補完機能の設定
;; ミニバッファ補完（vertico等）とバッファ内補完（corfu/cape）の設定。

;;;; Completion UI
;;;; Vertico - ミニバッファ補完
;;"入力補完の候補をTABを押さずとも一覧から選べるようにする
 ;; https://github.com/minad/vertico

(use-package vertico
  :init
  (vertico-mode))

;;;; Orderless - スペース区切りあいまい検索
;; 入力補完の際、複数の語句で検索できるようにする
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil))

;;;; Marginalia - 候補に説明を追加
;; 入力補完の候補に説明文を表示する
(use-package marginalia
  :init
  (marginalia-mode))

;;;; Consult - 高機能検索・移動コマンド
;; - M-sがconsultの検索のデフォルトプリフィックスと重なるのでC-c sに変更

(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (("C-s" . consult-line)
         ;; C-c bindings in `mode-specific-map'
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

;;;; Embark - 候補に対するアクション
(use-package embark
  :bind
  (("C-." . embark-act)))

;;;; Embark-Consult - EmbarkとConsultの連携
(use-package embark-consult
  :after (embark consult)

  ;; Embark Collect バッファで Consult プレビューを有効化
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; Which-Key - キーバインド候補表示
(use-package which-key
  :config
  (which-key-mode))

;;;; Corfu - バッファ内でのコード自動補完
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

  :custom
  (corfu-auto t)                        ; 自動で補完候補をポップアップ
  (corfu-auto-delay 0.0)                ; 遅延なし
  (corfu-auto-prefix 1)                 ; 1文字入力で発動

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

;;;; Cape - Corfu用の補完ソース
;; Abbrev (または abbr.) は、英語の abbreviation（省略、略語、短縮形）の略
;; C-c p は projectile のために空ける

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  ;; :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :bind ("M-<tab>" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev) ; 開いているバッファの単語補完
  (add-hook 'completion-at-point-functions #'cape-file)    ; ファイルパス補完
  (add-hook 'completion-at-point-functions #'cape-history) ; ミニバッファ履歴補完
  (add-hook 'completion-at-point-functions #'cape-symbol) ; Emacs Lispシンボル補完
  (add-hook 'completion-at-point-functions #'cape-elisp-block) ; OrgやMarkdown中のElispコード補完
  (add-hook 'completion-at-point-functions #'cape-keyword) ; プログラミング言語の予約語補完
  (add-hook 'completion-at-point-functions #'cape-dict)  ; 辞書による英単語補完
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

;;; Orgモードの設定
;; Org本体・キャプチャ・org-roam・見た目など、Orgで書く・管理するための設定。
;; エクスポート関連は「文書作成・エクスポートの設定」節。

;;;; org - 本体の設定
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

;;;; org-reverse-datetree - キャプチャする際の日付を降順にする
;; https://github.com/akirak/org-reverse-datetree
(use-package org-reverse-datetree
  :after org)

;;;; doct - org-captureの設定
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
                 (("blog.chubachi.net"  :keys "b"
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

;;;; org-sidebar - Orgの構造をサイドバーに表示
(use-package org-sidebar
  :bind ("C-c t" . org-sidebar-tree)
  :custom
  (org-sidebar-tree-side 'left))

;;;; markdown-mode - org-src内でMarkdownをハイライトするために使用
(use-package markdown-mode
  :ensure t)

;;;; org-tempo - #begin_...を簡単に
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
               '("md" . "src markdown"))
  (add-to-list 'org-structure-template-alist
               '("n" . "note"))
  (add-to-list 'org-structure-template-alist
               '("w" . "warning"))
  (add-to-list 'org-structure-template-alist
               '("f" . "figure"))
  (add-to-list 'org-structure-template-alist
               '("ai" . "ai")))

;;;; TODO: クリップボードのMarkdownテキストをOrg-mode形式に変換して貼り付け

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

;;;; org-roam - 個人知識ベース（Zettelkasten）
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Dropbox/Org/Roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (require 'org-roam-protocol))

;;;; org-roam-ui - org-roamをグラフ表示するWeb UI
(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;;;; org-modern - Org-modeの見た目を近代的に
(use-package org-modern
  :ensure t
  :custom
  (org-modern-list '((?+ . "◦") (?- . "-") (?* . "•")))
  (org-modern-star '("■" ".◆" "..●" "...＊" "....＋"))
  :config
  ;; 余白と境界線の設定は「見た目の設定」節
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

;;;; org-download - 画像のドラッグ＆ドロップ挿入
(use-package org-download
  :ensure t
  :custom
  (org-download-method 'attach)
  :config
  (setq org-image-actual-width 400)
  (add-hook 'dired-mode-hook #'org-download-enable)
  (when (eq system-type 'windows-nt)
    (setq org-download-screenshot-method "magick convert clipboard: %s")))

;;;; visual-fill-column - org-modeでの折返し表示
(use-package visual-fill-column
  :ensure t
  :hook (org-mode . visual-fill-column-mode)
  :bind (("C-c q" . visual-fill-column-mode)
         (:map visual-fill-column-mode-map
               ("C-a" . beginning-of-visual-line)
               ("C-e" . end-of-visual-line)
               ("C-k" . kill-visual-line))))

;;;; ob-plantuml - PlantUMLによる図表生成
(use-package ob-plantuml
  :ensure nil ; org-babel 内蔵の plantuml 統合を使用
  :after org
  :config
  ;; plantuml.jarへのパスを設定
  (setq org-plantuml-jar-path (expand-file-name "lib/plantuml-1.2022.12.jar" user-emacs-directory))
  ;; org-babelで使用する言語を登録
  (add-to-list 'org-babel-load-languages '(plantuml . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;;;; ----------------------------------------------------------
;;;; 移さなくて良いもの（コメントアウト）
;;;; ----------------------------------------------------------
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

;;; 文書作成・エクスポートの設定
;; LaTeX（AUCTeX）・Pandoc・Orgのエクスポート（LaTeX/HTML/publish）とプレビューの設定。

;;;; LaTeX - AUCTeXの利用（Corfuと連携可）

;; 近年Emacsコミュニティで主流になっている、軽量で動作が非常に滑らかな Corfu を使う方法です。
;; こちらはAUCTeXが標準で提供する補完機能（completion-at-point）をそのまま綺麗にポップアップ化するため、追加の連携パッケージが不要で動作が極めて高速です。

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;;;; pandoc-mode - Pandoc経由の文書変換
(use-package pandoc-mode
  :ensure t
  :after hydra
  :commands pandoc-mode)

;;;; org-preview-mode
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

;;;; ox-latex - LaTeXエクスポート設定

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
  (org-latex-compiler      "lualatex")
  (org-latex-pdf-process   '("latexmk -f -gg -pvc- -%latex %f"))
  (org-latex-default-class "jlreq")
  (org-latex-hyperref-template
   "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},pdfsubject={%d},\n pdfcreator={%c},\n pdflang={Japanese},\n colorlinks={true},linkcolor={blue}\n}\n")
  (org-latex-listings 'minted)
  (org-latex-minted-options
   '(("frame" "lines")
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

  ;; 日本語の途中で改行してもPDF出力時に余分な半角スペースが入らないようにする
  (defun my/org-latex-filter-nospace-japanese (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "\\([ぁ-んーァ-ヶー一-龠]\\)\n\\([ぁ-んーァ-ヶー一-龠]\\)" "\\1\\2" text)))
  (add-to-list 'org-export-filter-paragraph-functions 'my/org-latex-filter-nospace-japanese))

;; (use-package ox-beamer
;;   :after ox-latex
;;   :custom
;;   (org-beamer-outline-frame-title . "目次")
;;   (org-beamer-frame-default-options . "t"))

;;;; 自作：GitプロジェクトをGitHub Pagesにパブリッシュする

;; src/ パブリッシュしたいOrgファイル一式
;; public/ HTMLファイルのパブリッシュ先

;; orgファイルを開いて M-x my/org-publish-current-sight を実行

(use-package ox-publish
  :ensure nil
  :after org
  :config
  (defun my/org-publish-current-site ()
    "開いているファイルの Git ルートを取得し、./src から ./public にパブリッシュする"
    (interactive)
    (let* ((root (or (vc-root-dir) default-directory))
           (src (expand-file-name "src/" root))
           (public (expand-file-name "public/" root))
           ;; ルートのパスからフォルダ名（例: "lecture-prog_mid"）のみを取得
           (site-name (file-name-nondirectory (directory-file-name root)))
           (project-name (concat "auto-site-" site-name)))
      (setq org-publish-project-alist
            `((,project-name
               :base-directory ,src
               :publishing-directory ,public
               :publishing-function org-html-publish-to-html
               :recursive t)))
      (org-publish-project project-name))))

;;;; ox-html - デフォルトでスタイルシートをつける

(with-eval-after-load 'ox-html
  ;; デフォルトの HTML ヘッダーに OrgCSS を指定
  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\" />
<style>body { font-family: \"Hiragino Sans\", \"Meiryo\", sans-serif !important; }</style>")
  ;; Emacs 標準の組み込みスタイル（インラインCSS）を出力しないようにする
  (setq org-html-head-include-default-style nil))

;;;; org-exportするときに日本語で改行したときの空白を削除する
(with-eval-after-load 'ox
  (defun my/org-export-remove-cjk-spaces (text backend info)
    "全角文字（日本語）間に挟まった改行とそれに伴う半角スペースを削除する"
    (when (org-export-derived-backend-p backend 'html)
      (let ((cjk "\\(?:\\cc\\|\\ck\\|\\ch\\|\\cA\\|\\cK\\|\\cC\\|\\cH\\)"))
        (replace-regexp-in-string
         (format "\\(%s\\)\n[ \t]*\\(%s\\)" cjk cjk)
         "\\1\\2" text))))

  (add-to-list 'org-export-filter-plain-text-functions
               'my/org-export-remove-cjk-spaces))

;;;; ox-pandoc - Pandoc経由のOrgエクスポート
(use-package ox-pandoc
  :ensure t
  :after org)

;;;; eww - org-preview-html-viewerで使うewwの見た目設定
(use-package eww
  :ensure nil
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-image-animate nil)
  (shr-width 72)
  (eww-search-prefix "https://www.google.com/search?q="))

;;; テキスト編集・ウィンドウ操作の設定
;; テキスト編集全般と、ウィンドウ・バッファ操作の設定。

;;;; undo-tree - C-zでUndoツリーを表示する
(use-package undo-tree
  :demand t
  :bind ("C-z" . undo-tree-visualize)
  :config
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;;;; outli - Orgぽく使えるアウトラインモード
;; https://github.com/jdtsmith/outli

(use-package outli
  ;; :after lispy ; uncomment only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading)))
              ("C-c C-n" . outline-next-visible-heading))
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer

;;;; multiple-cursors - 複数カーソル同時編集
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

;;;; yasnippet - テンプレート挿入機能
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :custom
  (yas-snippet-dirs (list (expand-file-name "etc/yasnippet/snippets" user-emacs-directory)))
  :hook
  (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;;; hydra - 複数キーの連続操作をまとめる
(use-package hydra
  :ensure t
  :config
  (defhydra hydra-buffer-menu (:color pink :hint nil)
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

;;;; ウィンドウ・バッファ操作

;;;;; ace-window - ウィンドウにラベルを表示して素早く移動・操作する
(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-window)
  :custom
  (aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)) ; Dvorak配列のホームポジション（QWERTYのa s d f g h j k lと同じ物理キー）
  (aw-scope 'frame)
  (aw-background t)
  :custom-face
  (aw-leading-char-face ((t (:height 3.0 :foreground "red")))))

;;;;; swap-buffers - 隣のウィンドウとバッファを入れ替え
(use-package swap-buffers
  :ensure t
  :bind
  ("C-c b" . swap-buffers)
  :custom
  ;; Dvorak配列のホームポジション（ace-windowと同じ考え方）
  (swap-buffers-qwerty-shortcuts '("a" "o" "e" "u" "i" "d" "h" "t" "n" "s" "-")))

;;;;; perspective - バッファをグループ化して切り替える
(use-package perspective
  :ensure t
  :bind
  (("C-x C-b" . persp-list-buffers))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (persp-mode 1))

;;; バージョン管理の設定
;; Git（magit・diff-hl）とEmacs標準VCの設定。

;;;; magit - Gitのフロントエンド
;; 内蔵されている古いtransientではなく、ELPA/MELPAの最新版を優先して読み込む
(use-package transient
  :defer t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)       ; 標準的なMagitの起動ショートカット
         ("C-x M-g" . magit-dispatch))  ; 各種Gitコマンドのポップアップ
  :config
  ;; 1. コミットメッセージ入力時に自動で折り返す（長文対策）
  (add-hook 'with-editor-mode-hook 'turn-on-auto-fill)

  ;; 2. 大規模リポジトリでの速度低下を防ぐ（Windows/Linux共通の高速化設定）
  (setq magit-refresh-status-buffer nil) ; バッファ切り替え時の自動更新を抑制

  ;; OSごとの個別最適化（Windows環境のMagitは遅くなりやすいため）
  (cond
   ((eq system-type 'windows-nt)
    ;; WindowsでMagitの挙動を高速化するハック
    (setq vc-handled-backends nil)    ; Emacs標準のVC機能を無効化してMagitに集中
    (setq magit-git-executable "git")) ; Gitのパスを明示（環境に応じてフルパスに）

   ((eq system-type 'gnu/linux)
    ;; Linux向けの特有設定があればここに記述
    nil)))

;;;; diff-hl - diffをわかり易く表示
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1)
  :hook
  ((dired-mode . diff-hl-dired-mode)         ; Dired でも変更状態を表示
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)) ; Magit 操作後に即座に表示を同期
  :bind
  (("C-c v n" . diff-hl-next-hunk)           ; 次の変更箇所へジャンプ
   ("C-c v p" . diff-hl-previous-hunk)       ; 前の変更箇所へジャンプ
   ("C-c v d" . diff-hl-diff-goto-hunk)      ; 該当箇所の diff を開く
   ("C-c v r" . diff-hl-revert-hunk)         ; カーソル位置の変更を元に戻す
   ("C-c v s" . diff-hl-stage-current-hunk)) ; カーソル位置の変更のみをステージング
  :config
  ;; フリンジの見た目を少し太く・見やすく調整（お好みで）
  ;; (setq diff-hl-draw-borders nil)

  ;; フリンジがない環境（ターミナル等）や Flymake/Flycheck とフリンジが衝突する場合はマージン表示へ自動切替
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

;;;; vc-hooks
(use-package vc-hooks
  :ensure nil
  :custom
  (vc-handled-backends '(Git))) ; Gitのみ使用

;;; プログラミング支援の設定
;; LSP・構文チェック・Lisp編集・プロジェクト管理など、プログラミング全般の設定。

;;;; 一般

;;;;; Syntax check

(use-package flymake
  :ensure nil
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

;;;;; LSP

(use-package eglot
  :ensure nil

  :hook
  ((python-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (typescript-mode . eglot-ensure))

  :custom
  (eglot-autoshutdown t))

;;;;; インデントガイド

(use-package highlight-indent-guides
  :hook
  ((prog-mode . highlight-indent-guides-mode)
   (yaml-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'column))

;;;; Lisp編集

;;;;; カッコの対応関係
;; M-sがconsultの検索のデフォルトプリフィックスと重なるのでconsult側で対応

(use-package paredit
  :vc (:url "https://github.com/emacsmirror/paredit") ; 2026/09/14 本家のサイトにアクセスできない
  :commands (paredit-mode)
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (scheme-mode . enable-paredit-mode)))

;;;;; 括弧を色分け

;; テーマによって色が設定されていない場合がある
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; 各階層（1〜9）の色を明示的に指定する例
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#E06C75") ; 赤
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#98C379") ; 緑
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#E5C07B") ; 黄
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#61AFEF") ; 青
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#C678DD") ; 紫
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#56B6C2") ; シアン
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#D19A66") ; オレンジ
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#BE5046") ; 濃赤
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#ABB2BF") ; グレー
  ;; 不整合エラーの括弧を強調
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "#FFFFFF" :background "#E06C75" :weight 'bold))

;;;;; マクロ展開

(use-package macrostep
  :bind
  (:map emacs-lisp-mode-map
        ("C-c e" . macrostep-expand)))

;;;; その他
;;;;; Dockerfile

(use-package dockerfile-mode
  :config
  (put 'dockerfile-image-name
       'safe-local-variable
       #'stringp))

;;;;; yaml-mode - YAMLファイルの編集
(use-package yaml-mode
  :ensure t)

;;;;; projectile - プロジェクト管理
;; cofu の バインディングと重なっていたため、cofu側を変更

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)
              ("C-c p s" . consult-ripgrep))
  :custom
  (projectile-project-search-path '("~/.emacs.d/" ("~/git" . 1)))
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  ;; ripgrep がインストールされている場合に優先使用
  (when (executable-find "rg")
    (setq projectile-generic-command "rg --files --hidden --glob '!.git'"))
  )

;;;;; consult-projectile
(use-package consult-projectile :ensure t :after projectile)

;;; AI関連の設定
;; AIエージェント関連の設定をまとめる。

;;;; agent-shell - AI(Claude Code)

(use-package agent-shell
  :config
  ;; --- 認証方法の設定（以下のいずれかを選択） ---

  ;; パターンA: Claude のサブスクリプションログインを使う場合（デフォルト）
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; パターンB: Anthropic API キーを使う場合
  ;; (setq agent-shell-anthropic-authentication
  ;;       (agent-shell-anthropic-make-authentication
  ;;        :api-key (lambda ()
  ;;                   (or (getenv "ANTHROPIC_API_KEY")
  ;;                       (setenv "ANTHROPIC_API_KEY" (read-passwd "ANTHROPIC_API_KEY: "))))))

  ;; デフォルトのエージェントを Claude Code に固定
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config)))

;;; その他の設定
;; 単独で使う小物ツールと、未整理・要確認の設定置き場。
;;
;; init.old.org（旧設定）からの移植候補のうち、現行の設定に
;; 既に取り込んだもの・明らかに不要と判断したものは削除済み。
;; 「未検討（保留）」「要確認」に残っているのは (1) 使うかどうか判断がつかない設定、
;; (2) 本人に利用状況を確認してから移植/削除を決めたい設定。

;;;; ツール

;;;;; shell-pop - ポップアップ型シェルバッファ

(use-package shell-pop
  :ensure t
  :bind
  (("C-c z" . shell-pop))
  :custom
  (shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
  (shell-pop-window-position "bottom")
  (shell-pop-window-size 30)
  (shell-pop-full-span t))

;;;;; free-keys - 空いているキーバインドを確認する
(use-package free-keys
  :ensure t
  :commands free-keys)

;;;; 未検討（保留）

;; (use-package display-fill-column-indicator
;;   :hook
;;   (emacs-startup-hook . global-display-fill-column-indicator-mode))

;; (use-package midnight
;;   :url "https://www.emacswiki.org/emacs/MidnightMode"
;;   :custom
;;   ((clean-buffer-list-delay-general . 1))
;;   :hook
;;   (emacs-startup-hook . midnight-mode))

;; (use-package whitespace
;;   :init
;;   (setq whitespace-style
;;         '(
;;           face                  ; faceで可視化
;;           trailing              ; 行末
;;           tabs                  ; タブ
;;           spaces                ; スペース
;;           space-mark            ; 表示のマッピング
;;           tab-mark
;;           ))
;;   (setq whitespace-display-mappings
;;         '(
;;           (space-mark ?\u3000 [?□])
;;           (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])
;;           ))
;;   (setq whitespace-trailing-regexp  "\\([ \u00A0]+\\)$")
;;   (setq whitespace-space-regexp "\\(\u3000+\\)")
;;   (global-whitespace-mode t))

;; (use-package imenu-list
;;   :bind (("C-c i" . imenu-list-smart-toggle))
;;   :hook
;;   (imenu-list-major-mode-hook . (lambda nil (display-line-numbers-mode -1))))

;; (add-hook 'org-mode-hook
;;           (lambda () (imenu-add-to-menubar "Imenu")))
;; (setq org-imenu-depth 3)
;; (add-hook 'org-mode-hook 'imenu-list-minor-mode)

;; (use-package moody
;;   :config
;;   (setq x-underline-at-descent-line t)
;;   (moody-replace-mode-line-buffer-identification)
;;   (moody-replace-vc-mode)
;;   (moody-replace-eldoc-minibuffer-message-function))

;; (use-package ruler-mode
;;   :config
;;   (add-hook 'find-file-hook (lambda () (ruler-mode 1))))

;;;; 要確認 - 本人に利用状況を確認してから移植/削除を決める

;; org-publish-project-alist (chubachi.net向け) - SCP経由のpublishを今も使うか
;; 「Orgモードの設定」節のmy/org-publish-current-siteは汎用の別方式
;; (use-package org-publish-project-alist
;;   :config
;;   (setq org-publish-project-alist
;;         '(("chubachi.net"
;;            :components ("chubachi.net-orgfiles" "chubachi.net-others"))
;;           ("chubachi.net-orgfiles"
;;            :publishing-function org-html-publish-to-html
;;            :base-directory "~/Dropbox/Org/publish/chubachi.net/"
;;            :publishing-directory "/scpx:chubachi@chubachi.sakura.ne.jp:~/www/chubachi.net/publish"
;;            :base-extension "org"
;;            :recursive t)
;;           ("chubachi.net-others"
;;            :publishing-function org-publish-attachment
;;            :base-directory "~/Dropbox/Org/publish/chubachi.net/"
;;            :publishing-directory "/scpx:chubachi@chubachi.sakura.ne.jp:~/www/chubachi.net/publish/"
;;            :base-extension "jpg\\|gif\\|png|css\\|el"
;;            :recursive t))))

(message "init.el loaded")
(provide 'init)
;;; init.el ends here
