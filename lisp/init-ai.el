;;; init-ai.el --- AI関連の設定  -*- lexical-binding: t; -*-
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

;; AIエージェント関連の設定をまとめる。

;;; Code:

;;; agent-shell - AI(Claude Code)

(use-package agent-shell
  :config
  ;; --- 認証方法の設定（以下のいずれかを選択） ---

  ;; パターンA: Claude のサブスクリプションログインを使う場合（デフォルト）
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t))

  ;; パターンB: Anthropic API キーを使う場合
  ;; (setq agent-shell-anthropic-authentication
  ;;       (agent-shell-anthropic-make-authentication
  ;;        :api-key (lambda ()
  ;;                   (or (getenv "ANTHROPIC_API_KEY")
  ;;                       (setenv "ANTHROPIC_API_KEY" (read-passwd "ANTHROPIC_API_KEY: "))))))

  ;; デフォルトのエージェントを Claude Code に固定
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config)))

(provide 'init-ai)
;;; init-ai.el ends here
