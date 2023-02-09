(define-key key-translation-map [?\C-h] [?\C-?])

(global-set-key (kbd "C-^") help-map)

(leaf *undo :bind (("C-z" . undo)))

(leaf mozc
  :straight t
  :config
  (cond ((eq system-type 'windows-nt)
      (setq mozc-helper-program-name "~/Dropbox/bin/mozc_emacs_helper.exe"))
      (t (setq mozc-helper-program-name "mozc_emacs_helper")))
  ;; (if (getenv "WSLENV")
  ;;     ;; (setq mozc-helper-program-name "mozc_emacs_helper_win.sh")
  ;;     (setq mozc-helper-program-name "mozc_emacs_helper")
  ;;   (setq mozc-helper-program-name "mozc_emacs_helper"))
  (leaf mozc-im
    :straight t
    :require t
    :custom ((default-input-method . "japanese-mozc-im"))
    :bind* (("C-o" . toggle-input-method))
    :config
    (setq mozc-candidate-style 'echo-area))
  (leaf mozc-cursor-color
    :straight (mozc-cursor-color :type git :host github
                                 :repo "iRi-E/mozc-el-extensions")
    :require t
    :config
    (setq mozc-cursor-color-alist
          '((direct        . "gray")
            (read-only     . "yellow")
            (hiragana      . "green")
            (full-katakana . "goldenrod")
            (half-ascii    . "dark orchid")
            (full-ascii    . "orchid")
            (half-katakana . "dark goldenrod")))
    ;; mozc-cursor-color を利用するための対策（NTEmacs@ウィキより）
    (defvar-local mozc-im-mode nil)
    (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
    (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
    (advice-add 'mozc-cursor-color-update
                :around (lambda (orig-fun &rest args)
                          (let ((mozc-mode mozc-im-mode))
                            (apply orig-fun args)))))
  (leaf *mozc-win
    :if (eq system-type 'windows-nt)
    :config
    (advice-add 'mozc-session-execute-command
  	  :after (lambda (&rest args)
  		   (when (eq (nth 0 args) 'CreateSession)
  		     (mozc-session-sendkey '(Hankaku/Zenkaku)))))))

(when (eq system-type 'windows-nt)
  ;; 通常使用するフォント
  (set-frame-font "PlemolJP-12" nil t)
  ;; 行間
  (setq-default line-spacing 0)
  ;; IME未確定時のフォント設定
  (modify-all-frames-parameters '((ime-font . "PlemolJP-12"))))
