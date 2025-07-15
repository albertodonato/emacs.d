;;; ack-ai.el --- Configuration for AI-related tools

;;; Commentary:
;;;   Configuration for AI-related tools.

;;; Code:

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "AIDER_ANTHROPIC_API_KEY" (ack/secret "token-Anthropic"))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet")
  (aidermacs-extra-args '("--no-gitignore" "--no-check-update")))

(provide 'ack-ai)
;;; ack-ai.el ends here
