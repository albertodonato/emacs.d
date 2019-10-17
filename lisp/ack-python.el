;;; ack-python.el --- Python editing configuration

;;; Commentary:
;;;   Configuration for python editing.

;;; Code:

(require 'python)
(require 'python-environment)

(setq python-environment-directory "~/virtualenv"
      python-environment-default-root-name "emacs"
      python-shell-interpreter "ipython"
      python-indent-offset 4)

(add-hook 'python-mode-hook #'lsp)

(provide 'ack-python)
;;; ack-python.el ends here
