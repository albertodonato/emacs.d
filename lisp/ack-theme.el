;;; ack-theme.el --- Theming configuration

;;; Commentary:
;;;   Themes related settings settings.

;;; Code:

(setq custom-theme-directory (concat user-emacs-directory "themes")
      custom-safe-themes t)

(use-package ample-theme
  :init
  (load-theme 'ample t))

(load-theme 'ack-ample t)
(load-theme 'ack-common t)

(provide 'ack-theme)
;;; ack-theme.el ends here
