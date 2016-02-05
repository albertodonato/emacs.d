;;; ack-packages.el --- Package configuration

;;; Commentary:
;;;   Package system and repositories configuration.

;;; Code:

(require 'package)

(setq package-user-dir "~/.emacs-packages"
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)
(package-install-selected-packages)

(provide 'ack-packages)
;;; ack-packages.el ends here
