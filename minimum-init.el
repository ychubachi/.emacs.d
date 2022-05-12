;; map backspace [delete-backward-char] to C-h
(define-key key-translation-map [?\C-h] [?\C-?])

(global-set-key (kbd "C-^") help-map)

(leaf *undo :bind (("C-z" . undo)))

(straight-use-package 'leaf)

(leaf *leaf-keywords
  :config
  (straight-use-package 'leaf-keywords)
  (leaf-keywords-init))

(leaf *leaf-optional-packages
    :config
    (leaf leaf-convert
      :straight t)
    (leaf leaf-tree
      :straight t
      :custom ((imenu-list-size . 30)
               (imenu-list-position . 'left)))
    (leaf hydra
      :straight t)
;;    (leaf el-get
;;      :straight t
;;      :custom ((el-get-git-shallow-clone  . t)))
    (leaf diminish
      :straight t))

(leaf mozc
  ;; :if (eq system-type 'gnu/linux)
  :straight t
  :config
  (if (eq system-type 'windows-nt)
      (setq mozc-helper-program-name "~/bin/mozc_emacs_helper.exe")
    (setq mozc-helper-program-name "mozc_emacs_helper"))
  ;; (if (getenv "WSLENV")
  ;;     ;; (setq mozc-helper-program-name "mozc_emacs_helper_win.sh")
  ;;     (setq mozc-helper-program-name "mozc_emacs_helper")
  ;;   (setq mozc-helper-program-name "mozc_emacs_helper"))
  (leaf mozc-im
    :straight t
    :require t
    :custom ((default-input-method . "japanese-mozc-im"))
    :bind* (("C-o" . enable-input-method)
            ("C-j" . disable-input-method))
    :config
    (defvar-local mozc-im-mode nil)
  
    (add-hook 'mozc-im-activate-hook
              (lambda nil
                (setq mozc-im-mode t)))
    (add-hook 'mozc-im-deactivate-hook
              (lambda nil
                (setq mozc-im-mode nil)))
  
    (defun enable-input-method (&optional arg interactive)
      (interactive "P\np")
      (if (not current-input-method)
          (toggle-input-method arg interactive)))
    (defun disable-input-method (&optional arg interactive)
      (interactive "P\np")
      (if current-input-method
          (toggle-input-method arg interactive)))
  
    (add-hook 'isearch-mode-hook
              (lambda () (setq im-state mozc-im-mode)))
    (add-hook 'isearch-mode-end-hook
              (lambda ()
                (unless (eq im-state mozc-im-mode)
                  (if im-state
                      (activate-input-method default-input-method)
                    (deactivate-input-method))))))
  
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
    (advice-add 'mozc-cursor-color-update :around
                (lambda (orig-fun &rest args)
                  (let ((mozc-mode mozc-im-mode))
                    (apply orig-fun args)))))
  (leaf mozc-posframe
    :straight (mozc-posframe :type git :host github :repo "derui/mozc-posframe")
    :config
    (mozc-posframe-register)
    (setq mozc-candidate-style 'posframe))
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
