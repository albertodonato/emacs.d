;;; ack-init.el --- Initialization config

;;; Commentary:
;;;   Base setup and helper.

;;; Code:

(use-package server
  :config
  (when (and (functionp 'server-running-p) (not (server-running-p)))
    (server-start)))

(defun ack/in-cache-dir (filename)
  "Return the full path for FILENAME in the cache directory."
  (concat (file-name-as-directory (concat user-emacs-directory "cache")) filename))

(use-package nsm
  :config
  (setq nsm-settings-file (ack/in-cache-dir "network-security.data")))

(use-package pcache
  :init
  (setq pcache-directory (ack/in-cache-dir "pcache")))

(use-package tramp-cache
  :config
  (setq tramp-persistency-file-name (ack/path-in-cache-dir "tramp")))

(use-package url-cookie
  :config
  (setq url-cookie-file (ack/in-cache-dir "cookies")))

(provide 'ack-init)
;;; ack-init.el ends here
