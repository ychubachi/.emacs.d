;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2022 Yoshihide Chubachi

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

;;  My init.el.

;;; Code:

(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(cond ((eq system-type 'windows-nt)
       (setq default-process-coding-system (cons 'utf-8 'cp932))))

;; Initialize packages.
(customize-set-variable
 'package-archives '(("org"   . "https://orgmode.org/elpa/")
                     ("melpa" . "https://melpa.org/packages/")
                     ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install Org with straight.el
;; Check Org version with M-x org-version.
(straight-use-package 'org)

;; Install leaf.el
(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(leaf-keywords-init)

;; Load my settings
(org-babel-load-file "~/.emacs.d/minimum-init.org")
(org-babel-load-file "~/.emacs.d/README.org")


;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; init.el ends here
