;;; init-document.el --- 文書作成・エクスポートの設定  -*- lexical-binding: t; -*-
;; Copyright (C) 2022-2026 Yoshihide Chubachi

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

;; LaTeX（AUCTeX）・Pandoc・Orgのエクスポート（LaTeX/HTML/publish）とプレビューの設定。

;;; Code:

;;; LaTeX - AUCTeXの利用（Corfuと連携可）

;; 近年Emacsコミュニティで主流になっている、軽量で動作が非常に滑らかな Corfu を使う方法です。
;; こちらはAUCTeXが標準で提供する補完機能（completion-at-point）をそのまま綺麗にポップアップ化するため、追加の連携パッケージが不要で動作が極めて高速です。

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;;; pandoc-mode - Pandoc経由の文書変換
(use-package pandoc-mode
  :ensure t
  :after hydra
  :commands pandoc-mode)

;;; org-preview-mode
;; https://github.com/jakebox/org-preview-html

(use-package org-preview-html
  :commands (org-preview-html-mode)
  :custom
  ;; プレビューの表示形式 ('eww または 'xwidget)
  (org-preview-html-viewer 'eww)
  ;; 更新タイミング ('save, 'export, 'timer, 'manual, 'instant)
  (org-preview-html-refresh-configuration 'save)
  ;; 'timer 設定時の更新間隔 (秒)
  (org-preview-html-timer-interval 2)
  :bind
  (:map org-mode-map
        ("C-c C-p" . org-preview-html-mode)))

;;; ox-latex - LaTeXエクスポート設定

;; #+TITLE: 日本語PDF出力テスト
;; #+AUTHOR: あなたの名前
;; #+LATEX_CLASS: bxjsarticle
;; #+LATEX_CLASS_OPTIONS: [a4paper,11pt]

;; 🚀 PDFの出力方法（キーバインド）
;; 1. Orgファイルを開いた状態で C-c C-e を押して、エクスポートディスパッチャーを開きます。
;; 2. l (Export to LaTeX) を選択します。
;; 3. p (As PDF file) を押してPDFを生成、または o (As PDF file and open) を押して生成後にビューアで開きます。

(use-package ox-latex
  :ensure nil
  :after org
  :custom
  (org-latex-compiler      "lualatex")
  (org-latex-pdf-process   '("latexmk -f -gg -pvc- -%latex %f"))
  (org-latex-default-class "jlreq")
  (org-latex-hyperref-template
   "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},pdfsubject={%d},\n pdfcreator={%c},\n pdflang={Japanese},\n colorlinks={true},linkcolor={blue}\n}\n")
  (org-latex-listings 'minted)
  (org-latex-minted-options
   '(("frame" "lines")
     ("framesep=2mm")
     ("linenos=true")
     ("baselinestretch=1.2")
     ("fontsize=\\footnotesize")
     ("breaklines")))
  :config
  (add-to-list
   'org-latex-classes
   '("jlreq"
     "\\documentclass{jlreq}"
     ("\\section{%s}"       . "\\section*{%s}")
     ("\\subsection{%s}"    . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}"     . "\\paragraph*{%s}")
     ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  (add-to-list
   'org-latex-classes
   '("jlreq-tate"
     "\\documentclass[tate]{jlreq}"
     ("\\section{%s}"       . "\\section*{%s}")
     ("\\subsection{%s}"    . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}"     . "\\paragraph*{%s}")
     ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  (add-to-list
   'org-latex-classes
   '("bxjsarticle"
     "\\documentclass{bxjsarticle}\n\\usepackage{luatexja}"
     ("\\section{%s}"       . "\\section*{%s}")
     ("\\subsection{%s}"    . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}"     . "\\paragraph*{%s}")
     ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))
  (add-to-list
   'org-latex-classes
   '("beamer"
     "\\documentclass[presentation]{beamer}\n\\usepackage{luatexja}\n\\renewcommand\\kanjifamilydefault{\\gtdefault}"
     ("\\section{%s}"       . "\\section*{%s}")
     ("\\subsection{%s}"    . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-packages-alist
               "\\usepackage{minted}" t)

  ;; 日本語の途中で改行してもPDF出力時に余分な半角スペースが入らないようにする
  (defun my/org-latex-filter-nospace-japanese (text backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (replace-regexp-in-string "\\([ぁ-んーァ-ヶー一-龠]\\)\n\\([ぁ-んーァ-ヶー一-龠]\\)" "\\1\\2" text)))
  (add-to-list 'org-export-filter-paragraph-functions 'my/org-latex-filter-nospace-japanese))

;; (use-package ox-beamer
;;   :after ox-latex
;;   :custom
;;   (org-beamer-outline-frame-title . "目次")
;;   (org-beamer-frame-default-options . "t"))

;;; 自作：GitプロジェクトをGitHub Pagesにパブリッシュする

;; src/ パブリッシュしたいOrgファイル一式
;; public/ HTMLファイルのパブリッシュ先

;; orgファイルを開いて M-x my/org-publish-current-sight を実行

(use-package ox-publish
  :ensure nil
  :after org
  :config
  (defun my/org-publish-current-site ()
    "開いているファイルの Git ルートを取得し、./src から ./public にパブリッシュする"
    (interactive)
    (let* ((root (or (vc-root-dir) default-directory))
           (src (expand-file-name "src/" root))
           (public (expand-file-name "public/" root))
           ;; ルートのパスからフォルダ名（例: "lecture-prog_mid"）のみを取得
           (site-name (file-name-nondirectory (directory-file-name root)))
           (project-name (concat "auto-site-" site-name)))
      (setq org-publish-project-alist
            `((,project-name
               :base-directory ,src
               :publishing-directory ,public
               :publishing-function org-html-publish-to-html
               :recursive t)))
      (org-publish-project project-name))))

;;; ox-html - デフォルトでスタイルシートをつける

(with-eval-after-load 'ox-html
  ;; デフォルトの HTML ヘッダーに OrgCSS を指定
  (setq org-html-head
        "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\" />
<style>body { font-family: \"Hiragino Sans\", \"Meiryo\", sans-serif !important; }</style>")
  ;; Emacs 標準の組み込みスタイル（インラインCSS）を出力しないようにする
  (setq org-html-head-include-default-style nil))

;;; org-exportするときに日本語で改行したときの空白を削除する
(with-eval-after-load 'ox
  (defun my/org-export-remove-cjk-spaces (text backend info)
    "全角文字（日本語）間に挟まった改行とそれに伴う半角スペースを削除する"
    (when (org-export-derived-backend-p backend 'html)
      (let ((cjk "\\(?:\\cc\\|\\ck\\|\\ch\\|\\cA\\|\\cK\\|\\cC\\|\\cH\\)"))
        (replace-regexp-in-string
         (format "\\(%s\\)\n[ \t]*\\(%s\\)" cjk cjk)
         "\\1\\2" text))))

  (add-to-list 'org-export-filter-plain-text-functions
               'my/org-export-remove-cjk-spaces))

;;; ox-pandoc - Pandoc経由のOrgエクスポート
(use-package ox-pandoc
  :ensure t
  :after org)

;;; eww - org-preview-html-viewerで使うewwの見た目設定
(use-package eww
  :ensure nil
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-image-animate nil)
  (shr-width 72)
  (eww-search-prefix "https://www.google.com/search?q="))

(provide 'init-document)
;;; init-document.el ends here
