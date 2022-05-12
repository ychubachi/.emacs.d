(setq debug-on-error t)

(prefer-coding-system 'utf-8)
(set-language-environment "Japanese")

(setq load-path (cons "~/.emacs.d/straight/build/mozc-im" load-path))
(setq load-path (cons "~/.emacs.d/straight/build/mozc" load-path))

(require 'mozc-im)

(setq mozc-helper-program-name "~/bin/mozc_emacs_helper.exe")
(setq default-input-method "japanese-mozc-im")
(global-set-key (kbd "C-o") 'toggle-input-method)


