;;; init.el --- Emacs init file

;;; Commentary:
;;;   Emacs default config file.
;;;   This is the entry point to load other config files.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'ack-setup)
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
