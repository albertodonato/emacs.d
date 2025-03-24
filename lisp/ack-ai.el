;;; ack-ai.el --- Configuration for AI-related tools

;;; Commentary:
;;;   Configuration for AI-related tools.

;;; Code:

(use-package aider
  :bind (("C-c a" . aider-transient-menu))
  :custom
  (aider-args '("--model" "sonnet" "--no-gitignore"))
  :config
  (setenv "ANTHROPIC_API_KEY" (ack/secret "token-Claude")))

(use-package gptel
  :custom
  (gptel-model 'claude-3-7-sonnet-latest)
  (gptel-backend (gptel-make-anthropic "Claude"
                   :stream t
                   :key (ack/secret "token-Claude"))))

(provide 'ack-ai)
;;; ack-ai.el ends here
