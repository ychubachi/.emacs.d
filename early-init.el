;;; early-init.el --- My early-init.el  -*- lexical-binding: t; -*-
;; https://github.com/emacscollective/no-littering/blob/master/README.org
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
	  (expand-file-name  "var/eln-cache/" user-emacs-directory))))
;;; early-init.el ends here
