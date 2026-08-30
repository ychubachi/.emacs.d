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

(provide 'init-editing)

;;; agent-shell - AI(Gemini)

(use-package agent-shell
  :config
  ;; --- 認証方法の設定（以下のいずれかを選択） ---

  ;; パターンA: Google API キーを使う場合
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication
         :api-key (getenv "GEMINI_API_KEY"))) ; 環境変数や文字列・関数で指定可能

  ;; パターンB: Google アカウントログイン (OAuth) を使う場合
  ;; (setq agent-shell-google-authentication
  ;;       (agent-shell-google-make-authentication :login t))

  ;; パターンC: Vertex AI を使う場合
  ;; (setq agent-shell-google-authentication
  ;;       (agent-shell-google-make-authentication :vertex-ai t))

  ;; デフォルトのエージェントを Gemini に固定したい場合（任意）
  (setq agent-shell-preferred-agent-config
        (agent-shell-google-make-gemini-config)))
