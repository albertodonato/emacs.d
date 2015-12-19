;; ack-modes.el --- Modes configuration

;;; Commentary:
;;;   Modes loading and configuration.

;;; Code:

(require 'secrets)
(require 'ox-reveal)
(require 'whitespace)

(require 'full-ack)
(setq ack-executable "/usr/bin/ack-grep"
      ack-use-environment nil)

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

(provide 'ack-modes)

;;; ack-modes.el ends here
