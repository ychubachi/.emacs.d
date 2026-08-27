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
