;;; outli
;; https://github.com/jdtsmith/outli

(use-package outli
  ;:after lispy ; uncomment only if you use lispy; it also sets speed keys on headers!
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading)))
              ("C-c C-n" . outline-next-visible-heading))
  :hook ((prog-mode text-mode) . outli-mode)) ; or whichever modes you prefer

(provide 'init-editing)
