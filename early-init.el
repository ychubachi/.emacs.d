;;; early-init.el --- My early-init.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2025 Yoshihide Chubachi

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

;; このファイルはearly-init.orgから生成します。

;;; Code:

(setq package-enable-at-startup nil)

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
           (expand-file-name
            "straight/repos/straight.el/bootstrap.el"
            (or (bound-and-true-p straight-base-dir)
                user-emacs-directory)))
          (bootstrap-version 7))
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

(leaf leaf-convert :straight t)

(leaf org :straight t)

(leaf no-littering :straight t :require t
        :url "https://github.com/emacscollective/no-littering#usage"
        :init
        (let ((dir (no-littering-expand-var-file-name "lock-files/")))
          (make-directory dir t)
          (setq lock-file-name-transforms `((".*" ,dir t))))

        (require 'recentf)
        (add-to-list 'recentf-exclude
                     (recentf-expand-file-name no-littering-var-directory))
        (add-to-list 'recentf-exclude
                     (recentf-expand-file-name no-littering-etc-directory))

        (custom-set-variables '(custom-file
                                (no-littering-expand-etc-file-name "custom.el"))))

(when (and (fboundp 'startup-redirect-eln-cache)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
(startup-redirect-eln-cache
 (convert-standard-filename
  (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(load-theme 'misterioso)

;;; early-init.el ends here
