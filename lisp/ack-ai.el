;;; ack-ai.el --- Configuration for AI-related tools

;;; Commentary:
;;;   Configuration for AI-related tools.

;;; Code:

(use-package agent-shell
  :bind (("C-c c" . agent-shell-anthropic-start-claude-code))
  :custom
  (agent-shell-anthropic-authentication (agent-shell-anthropic-make-authentication :login t))
  (agent-shell-anthropic-claude-command '("claude-agent-acp"))
  (shell-maker-prompt-before-killing-buffer nil))

(provide 'ack-ai)
;;; ack-ai.el ends here
