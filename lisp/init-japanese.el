(use-package emacs
  :ensure nil
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8)
  (cond ((eq system-type 'windows-nt)
	 (setq default-process-coding-system
	       (cons 'utf-8 'cp932-unix)))))

(use-package emacs
  :ensure nil
  :config
  ;; ｜あいうえお｜
  ;; ｜憂鬱な檸檬｜
  ;; ｜<miilwiim>｜
  ;; ｜!"#$%&'~{}｜
  ;; ｜🙆iimmiim>｜
  (custom-set-faces
   '(default ((t (:family "HackGen"))))
   ;; '(default ((t (:family "Noto Sans"))))
   ;; '(default ((t (:family "PlemolJP"))))
   ;; '(default ((t (:family "UDEV Gothic NF"))))
   ;; '(default ((t (:family "FirgeNerd"))))
   ))

(use-package mozc
  :demand t
  :config
  (cond
   ((eq system-type 'windows-nt)
    ;; helperのVer 1.13
    ;; (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper-1.13.exe"))
    (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper-2.31.exe"))
   (t
    ;; helperのVer 2.31
    (setq mozc-helper-program-name "mozc_emacs_helper.sh"))))

(use-package mozc-im
  :after mozc
  :demand t
  :bind
  (("C-o" . toggle-input-method))
  :init
  (setq default-input-method "japanese-mozc-im"))

;;; TODO
;; (use-package mozc-cursor-color
;;   :elpaca (mozc-cursor-color :host github :repo "iRi-E/mozc-el-extensions" :files ("mozc-cursor-color.el"))
;;   :demand t
;;   :after mozc-im
;;   :hook (after-init . mozc-cursor-color-setup)
;;   :config
;;   ;; 入力状態に応じたカーソル色の設定（カラーコードや色名で指定）
;;   (setq mozc-cursor-color-alist
;;         '((direct . "white")     ; 英語入力（IME OFF）の時の色
;;           (hiragana . "cyan")    ; ひらがな入力（IME ON）の時の色
;;           (read-only . "red")))) ; 読み取り専用バッファの時の色

;; (use-package emacs
;;   :ensure nil
;;   :if (eq system-type 'windows-nt)
;;   ;; :defun (mozc-session-sendkey)
;;   :config
;;   (advice-add 'mozc-session-execute-command
;;               :after (lambda (&rest args)
;;                        (when (eq (nth 0 args) 'CreateSession)
;;                          (mozc-session-sendkey '(Hankaku/Zenkaku))))))

(provide 'init-japanese)
