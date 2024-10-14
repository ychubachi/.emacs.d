;;; init.el --- My init.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2024 Yoshihide Chubachi

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

;; このファイルはinit.orgから生成します。

;;; Code:

(eval-and-compile
  (prog1 "package"
    (customize-set-variable
     'package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize))

  (prog1 "straight.el"
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 6))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage)))

  (prog1 "leaf"
    (straight-use-package 'leaf)
    (straight-use-package 'leaf-keywords)
    (leaf-keywords-init)))

(eval-when-compile
  (require 'org)
  (org-babel-tangle-file (expand-file-name "early-init.org" user-emacs-directory))
  (byte-compile-file (expand-file-name "early-init.el" user-emacs-directory))
  (org-babel-tangle-file (expand-file-name "config.org" user-emacs-directory))
  (byte-compile-file (expand-file-name "config.el" user-emacs-directory)))

(load-file "config.el")

(provide 'init.el)
;;; init.el ends here
