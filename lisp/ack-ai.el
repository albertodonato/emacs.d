;;; ack-ai.el --- Configuration for AI-related tools

;;; Commentary:
;;;   Configuration for AI-related tools.

;;; Code:

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "ANTHROPIC_API_KEY" (ack/secret "token-Claude"))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))

(provide 'ack-ai)
;;; ack-ai.el ends here
