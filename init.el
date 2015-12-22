;;; init.el --- Emacs init file

;;; Commentary:
;;;   Emacs default config file.
;;;   This is the entry point to load other config files.

;;; Code:

; (package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "extra"))

(require 'ack-init)
(require 'ack-custom)
(require 'ack-private)
(require 'ack-packages)
(require 'ack-backup)
(require 'ack-interactive)
(require 'ack-editing)
(require 'ack-python)
(require 'ack-modes)
(require 'ack-bindings)
(require 'ack-irc)
(require 'ack-automode)
(require 'ack-appearance)
(require 'ack-landscape)

(provide 'init)

;;; init.el ends here
