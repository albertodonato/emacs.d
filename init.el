;;; init.el --- Emacs init file

;;; Commentary:
;;;   Emacs default config file.
;;;   This is the entry point to load other config files.

;;; Code:

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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
