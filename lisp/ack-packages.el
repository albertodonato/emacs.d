;;; ack-packages.el --- Package configuration

;;; Commentary:
;;;   Package system and repositories configuration.

;;; Code:

(require 'package)

(setq package-user-dir "~/.emacs-packages"
      package-gnupghome-dir (expand-file-name "gnupg" package-user-dir)
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(package-install-selected-packages)

(provide 'ack-packages)
;;; ack-packages.el ends here
