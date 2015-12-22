;;; ack-appearance.el --- Appearance settings

;;; Commentary:
;;;   Appearance and themes settings.

;;; Code:

(setq inhibit-startup-message t
      initial-scratch-message nil
      inhibit-startup-echo-area-message "ack")
(setq initial-frame-alist '((fullscreen . maximized)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; display Emacs version and buffer name in window title
(setq frame-title-format
      '("Emacs " emacs-version ": "
        (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(winner-mode t)

(global-hl-line-mode t)
(column-number-mode t)
(which-function-mode t)
(show-paren-mode t)

;; theme configuration
(setq custom-theme-directory (concat user-emacs-directory "themes")
      custom-safe-themes t)
(load-theme 'ack t)

(provide 'ack-appearance)

;;; ack-appearance.el ends here
