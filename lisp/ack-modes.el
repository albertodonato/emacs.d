;; ack-modes.el --- Modes configuration

;;; Commentary:
;;;   Modes loading and configuration.

;;; Code:

(require 'autoinsert)
(require 'secrets)
(require 'ox-reveal)
(require 'whitespace)

(require 'ag)
(setq ag-highlight-search t
      ag-group-matches t
      ag-context-lines 3
      ag-arguments '("--stats" "--"))

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)

;; sgml-mode
(defun sgml-delete-tag--reindent (ARG)
  "After deleting a tag, indent properly.  ARG is ignored."
  (indent-region (point-min) (point-max)))
(advice-add 'sgml-delete-tag :before #'sgml-delete-tag--reindent)

(require 'tramp-cache)
(setq tramp-persistency-file-name (file-path-in-cache-dir "tramp"))

(require 'nsm)
(setq nsm-settings-file (file-path-in-cache-dir "network-security.data"))

(require 'abbrev)
(setq abbrev-file-name (file-path-in-cache-dir "abbrev_defs"))

(require 'pcache)
(setq pcache-directory (file-path-in-cache-dir "pcache"))

(provide 'ack-modes)
;;; ack-modes.el ends here
