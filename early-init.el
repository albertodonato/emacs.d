;;; early-init.el --- Emacs early init file

;;; Commentary:
;;;   Initialize global settings before calling init.el

;;; Code:

(setq default-frame-alist '((font . "Ubuntu Mono-7.5")
                            (fullscreen . maximized)
                            (vertical-scroll-bars))
      initial-frame-alist default-frame-alist
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs " emacs-version ": "
                           (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))
      initial-scratch-message nil
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      package-enable-at-startup nil)
(setq-default indicate-empty-lines t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(winner-mode t)
(global-hl-line-mode t)
(column-number-mode t)
(which-function-mode t)
(show-paren-mode t)

(provide 'early-init)
;;; early-init.el ends here
