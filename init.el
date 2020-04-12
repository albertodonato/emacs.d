;;; init.el --- Emacs init file

;;; Commentary:
;;;   Emacs default config file.
;;;   This is the entry point to load other config files.

;;; Code:

;; load custom first so `package' finds the list of packages to install
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; setup package repositories and ensure packages are installed
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(package-install-selected-packages)

(require 'use-package)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'ack-init)
(require 'ack-theme)
(require 'ack-interface)
(require 'ack-editing)
(require 'ack-interactive)
(require 'ack-bindings)
(require 'ack-backup)
(require 'ack-devel)
(require 'ack-languages)
(require 'ack-private)
(require 'ack-irc)
(provide 'init)
;;; init.el ends here
