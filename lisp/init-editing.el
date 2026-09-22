;;; outli - Orgぽく使えるアウトラインモード
;; https://github.com/jdtsmith/outli

(use-package outli
  ;:after lispy ; uncomment only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading)))
              ("C-c C-n" . outline-next-visible-heading))
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer

;;; LeTeX - AUCTeXの利用（Cofuと連携可）

;; 近年Emacsコミュニティで主流になっている、軽量で動作が非常に滑らかな Corfu を使う方法です。
;; こちらはAUCTeXが標準で提供する補完機能（completion-at-point）をそのまま綺麗にポップアップ化するため、追加の連携パッケージが不要で動作が極めて高速です。

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;;; yasnippet - テンプレート挿入機能
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

;;; shell-pop - ポップアップ型シェルバッファ

(use-package shell-pop
  :ensure t
  :bind
  (("C-c z" . shell-pop))
  :custom
  (shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda () (ansi-term shell-pop-term-shell))))
  (shell-pop-window-position "bottom")
  (shell-pop-window-size 30)
  (shell-pop-full-span t))

;;; multiple-cursors - 複数カーソル同時編集
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

;;; swap-buffers - 隣のウィンドウとバッファを入れ替え
(use-package swap-buffers
  :ensure t
  :bind
  ("C-c b" . swap-buffers))

;;; ace-window - ウィンドウにラベルを表示して素早く移動・操作する
(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)) ; Dvorak配列のホームポジション（QWERTYのa s d f g h j k lと同じ物理キー）
  (aw-scope 'frame)
  (aw-background t)
  :custom-face
  (aw-leading-char-face ((t (:height 3.0 :foreground "red")))))

(provide 'init-editing)
