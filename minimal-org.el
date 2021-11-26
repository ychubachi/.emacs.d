;;; Minimal setup to load latest `org-mode'.
;;; https://orgmode.org/org.html#Installation

;;; emacs -Q -l /path/to/minimal-org.el

;; Activate debugging.
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; Add latest Org mode to load path.
(add-to-list 'load-path (expand-file-name "https://orgmode.org/org.html#Installation"))
