;;; ack-setup.el --- Basic setup

;;; Commentary:
;;;   Base configuration and helpers.

;;; Code:

(require 'cl-extra)

(defun ack/in-cache-dir (filename)
  "Return the full path for FILENAME in the cache directory."
  (expand-file-name (concat "cache/" filename) user-emacs-directory))

(defun ack/available-executable (names)
  "Return the first available executable from a list."
  (cl-some
   (lambda (name) (if (executable-find name) name))
   names))

(use-package custom
  :ensure nil
  :config
  (setq custom-file (concat user-emacs-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :ensure nil
  :config
  (when (and (functionp 'server-running-p) (not (server-running-p)))
    (server-start)))

(use-package nsm
  :ensure nil
  :custom
  (nsm-settings-file (ack/in-cache-dir "network-security.data")))

(use-package tramp-cache
  :ensure nil
  :custom
  (tramp-persistency-file-name (ack/in-cache-dir "tramp")))

(use-package url-cookie
  :ensure nil
  :custom
  (url-cookie-file (ack/in-cache-dir "cookies")))

(use-package request
  :custom
  (request-storage-directory (ack/in-cache-dir "request")))

;; use pipes for subprocess communication
(setq process-connection-type nil)
;; optimizations for lsp-mode
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 5 1024 1024))

(provide 'ack-setup)
;;; ack-setup.el ends here
