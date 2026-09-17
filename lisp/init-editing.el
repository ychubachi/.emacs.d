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

;;; agent-shell - AI(Gemini)

(use-package agent-shell
  :config
  ;; --- 認証方法の設定（以下のいずれかを選択） ---

  ;; パターンA: Google API キーを使う場合
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication
         :api-key (lambda () % 起動時に直接環境変数を探しにいくのではなく、関数（lambda）として渡すことで、実際にAPIキーが必要になったタイミングでこの処理が実行される
                    (or (getenv "GEMINI_API_KEY")
                        (setenv "GEMINI_API_KEY" (read-passwd "GEMINI_API_KEY: ")))))) ; 環境変数がない場合はプロンプトで入力

  ;; パターンB: Google アカウントログイン (OAuth) を使う場合
  ;; (setq agent-shell-google-authentication
  ;;       (agent-shell-google-make-authentication :login t))

  ;; パターンC: Vertex AI を使う場合
  ;; (setq agent-shell-google-authentication
  ;;       (agent-shell-google-make-authentication :vertex-ai t))

  ;; デフォルトのエージェントを Gemini に固定したい場合（任意）
  (setq agent-shell-preferred-agent-config
        (agent-shell-google-make-gemini-config)))

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

(provide 'init-editing)
