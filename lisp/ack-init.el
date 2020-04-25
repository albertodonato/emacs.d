;;; ack-init.el --- Initialization config

;;; Commentary:
;;;   Base setup and helper.

;;; Code:

(use-package server
  :ensure nil
  :config
  (when (and (functionp 'server-running-p) (not (server-running-p)))
    (server-start)))

(defun ack/in-cache-dir (filename)
  "Return the full path for FILENAME in the cache directory."
  (expand-file-name (concat "cache/" filename) user-emacs-directory))

(use-package nsm
  :ensure nil
  :config
  (setq nsm-settings-file (ack/in-cache-dir "network-security.data")))

(use-package pcache
  :ensure nil
  :init
  (setq pcache-directory (ack/in-cache-dir "pcache")))

(use-package tramp-cache
  :ensure nil
  :config
  (setq tramp-persistency-file-name (ack/in-cache-dir "tramp")))

(use-package url-cookie
  :ensure nil
  :config
  (setq url-cookie-file (ack/in-cache-dir "cookies")))

(provide 'ack-init)
;;; ack-init.el ends here
