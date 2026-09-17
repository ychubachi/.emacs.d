;;; 言語環境と文字コードを設定する
(use-package emacs
  :ensure nil
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (cond ((eq system-type 'windows-nt)
	 (setq default-process-coding-system
	       (cons 'utf-8 'cp932-unix)))))

;;; 文字フォントを設定する
;; Ubuntuの場合、~/.fontsに必要なフォントを入れて
;; # fc-cache -fv
;; を実行

;; ｜あいうえお｜
;; ｜憂鬱な檸檬｜
;; ｜<miilwiim>｜
;; ｜!"#$%&'~{}｜
;; ｜🙆iimmiim>｜
(use-package emacs
  :ensure nil
  :config
  (custom-set-faces
   '(default ((t (:family "HackGen")))) ;; (x-list-fonts "HackGen") で確認可能
   ))

;;; mozc - 日本語変換用ヘルパーの呼び出し設定
;; WSL(Ubuntu) から利用する場合:
;; - mozc_emacs_helper.sh を作成し、Windows側のexeを呼び出す
(use-package mozc
  :demand t
  :config
  (cond
   ((eq system-type 'windows-nt)
    (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper-2.31.exe"))
   (t
    ;; helperのVer 2.31
    (setq mozc-helper-program-name "mozc_emacs_helper.sh"))))

;;; mozc-im - インプット方式の設定
(use-package mozc-im
  :after mozc
  :demand t
  :bind
  (("C-o" . toggle-input-method))
  :init
  (setq default-input-method "japanese-mozc-im")

  ;; モードによってカーソルの色を変える
  (defvar my/cursor-color-japanese "cyan")
  (defvar my/cursor-color-default (frame-parameter nil 'cursor-color))
  (add-hook 'input-method-activate-hook
            (lambda () (set-cursor-color my/cursor-color-japanese)))
  (add-hook 'input-method-deactivate-hook
            (lambda () (set-cursor-color my/cursor-color-default)))
  )

(provide 'init-japanese)
