;;; ack-setup.el --- Basic setup

;;; Commentary:
;;;   Base configuration and helpers.

;;; Code:

(defun ack/in-cache-dir (filename)
  "Return the full path for FILENAME in the cache directory."
  (expand-file-name (concat "cache/" filename) user-emacs-directory))

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
  :config
  (setq nsm-settings-file (ack/in-cache-dir "network-security.data")))

(use-package tramp-cache
  :ensure nil
  :config
  (setq tramp-persistency-file-name (ack/in-cache-dir "tramp")))

(use-package url-cookie
  :ensure nil
  :config
  (setq url-cookie-file (ack/in-cache-dir "cookies")))

(provide 'ack-setup)
;;; ack-setup.el ends here
