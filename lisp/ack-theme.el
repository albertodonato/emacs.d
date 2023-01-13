;;; ack-theme.el --- Theming configuration

;;; Commentary:
;;;   Themes related settings settings.

;;; Code:

(setq custom-theme-directory (concat user-emacs-directory "themes")
      custom-safe-themes t)

(use-package color-theme-sanityinc-tomorrow
  :init (load-theme 'sanityinc-tomorrow-night t))

(load-theme 'ack-sanityinc-tomorrow)

(provide 'ack-theme)
;;; ack-theme.el ends here
