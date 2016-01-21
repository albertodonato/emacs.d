;;; ack-init.el --- Initialization helpers

;;; Commentary:
;;;   Helper functions and variables for Emacs initialization.

;;; Code:

(defvar cache-dir (concat user-emacs-directory "cache"))

(defun file-path-in-cache-dir (filename)
  "Return the full path for FILENAME in the cache directory."
  (concat (file-name-as-directory cache-dir) filename))

(provide 'ack-init)

;;; ack-init.el ends here
