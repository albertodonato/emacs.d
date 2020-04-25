;;; early-init.el --- Emacs early init file

;;; Commentary:
;;;   Initialize global settings before calling init.el

;;; Code:


;; setup package repositories
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/"))
      package-enable-at-startup nil
      load-prefer-newer t)
(package-initialize)

;; install use-package only
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))
(require 'use-package)
(setq use-package-always-defer t
      use-package-always-ensure t)

;; setup defaults for frame and appearance before the frame itself is created
(setq default-frame-alist '((font . "Ubuntu Mono-7.5")
                            (fullscreen . maximized)
                            (vertical-scroll-bars))
      initial-frame-alist default-frame-alist
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs " emacs-version ": "
                           (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b")))
      initial-scratch-message nil
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq-default indicate-empty-lines t)

(add-hook 'focus-out-hook #'garbage-collect)

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
