;; ack-modes.el --- Modes configuration

;;; Commentary:
;;;   Modes loading and configuration.

;;; Code:

(require 'autoinsert)
(require 'secrets)
(require 'ox-reveal)
(require 'whitespace)

(require 'compile)
(setq compilation-scroll-output 'first-error)

(require 'ag)
(setq ag-highlight-search t
      ag-group-matches t
      ag-context-lines 3
      ag-search-stats t)

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)

(require 'sgml-mode)
;; reindent after tag is removed
(advice-add 'sgml-delete-tag :after '(lambda (arg) (indent-region (point-min) (point-max))))

(require 'tramp-cache)
(setq tramp-persistency-file-name (file-path-in-cache-dir "tramp"))

(require 'nsm)
(setq nsm-settings-file (file-path-in-cache-dir "network-security.data"))

(require 'abbrev)
(setq abbrev-file-name (file-path-in-cache-dir "abbrev_defs"))

(require 'pcache)
(setq pcache-directory (file-path-in-cache-dir "pcache"))

(require 'projectile)
(projectile-global-mode)
(setq projectile-known-projects-file (file-path-in-cache-dir "projectile-bookmarks.eld"))

(provide 'ack-modes)
;;; ack-modes.el ends here
