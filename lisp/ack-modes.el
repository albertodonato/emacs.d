;; ack-modes.el --- Modes configuration

;;; Commentary:
;;;   Modes loading and configuration.

;;; Code:

(require 'secrets)
(require 'ox-reveal)

;; full-ack
(require 'full-ack)
(setq ack-executable "/usr/bin/ack-grep"
      ack-use-environment nil)

(provide 'ack-modes)

;;; ack-modes.el ends here
