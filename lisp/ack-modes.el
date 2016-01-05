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

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

;; sgml-mode
(defadvice sgml-delete-tag (after reindent activate)
  "After deleting a tag, indent properly."
  (indent-region (point-min) (point-max)))

(require 'tramp-cache)
(setq tramp-persistency-file-name (file-path-in-cache-dir "tramp"))

(require 'nsm)
(setq nsm-settings-file (file-path-in-cache-dir "network-security.data"))

(provide 'ack-modes)

;;; ack-modes.el ends here
