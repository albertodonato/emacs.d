;;; ack-python.el --- Python editing configuration

;;; Commentary:
;;;   Configuration for python editing.

;;; Code:

(require 'python)
(require 'python-environment)
(require 'jedi)
(require 'jedi-direx)

(setq python-environment-directory "~/virtualenv"
      python-environment-default-root-name "emacs"
      python-shell-interpreter "ipython")

(add-hook 'python-mode-hook
          (lambda ()
            (setq jedi:tooltip-method nil
                  jedi:server-command (list (format "%s/%s/bin/jediepcserver"
                                                    python-environment-directory
                                                    python-environment-default-root-name)))
            (jedi:setup)
            (superword-mode 1)))

(add-hook 'jedi-mode-hook
          (lambda ()
            (jedi-direx:setup)
            (setq jedi:complete-on-dot t)))

(provide 'ack-python)

;;; ack-python.el ends here
