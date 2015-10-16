;;; ack-python.el --- Python editing configuration

;; Copyright (C) 2015-  Alberto Donato

;; Author: Alberto Donato <alberto.donato@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configuration for python editing.

;;; Code:

(require 'python)
(require 'python-environment)
(require 'jedi)
(require 'jedi-direx)

(setq python-environment-directory "~/virtualenv"
      python-environment-default-root-name "emacs"
      python-shell-interpreter "ipython"
      python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
      python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(add-hook 'python-mode-hook
          (lambda ()
            (setq jedi:tooltip-method nil
                  jedi:server-command (cons (format "%s/%s/bin/jediepcserver"
                                                    python-environment-directory
                                                    python-environment-default-root-name)
                                            '()))
            (jedi:setup)
            (superword-mode 1)))

(add-hook 'jedi-mode-hook
          (lambda ()
            (jedi-direx:setup)
            (setq jedi:complete-on-dot t)))

(provide 'ack-python)

;;; ack-python.el ends here
