;;; ack-ai.el --- Configuration for AI-related tools

;;; Commentary:
;;;   Configuration for AI-related tools.

;;; Code:

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" (ack/secret "token-Anthropic"))
  :custom
  (aidermacs-default-model "anthropic/claude-sonnet-4-5")
  (aidermacs-watch-files t)
  (aidermacs-extra-args '("--no-gitignore" "--no-check-update")))


(provide 'ack-ai)
;;; ack-ai.el ends here
