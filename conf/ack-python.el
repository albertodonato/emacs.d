;;; ack-python.el --- Python editing configuration

;; Copyright (C) 2015  Alberto Donato

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
(require 'jedi)

(add-hook 'python-mode-hook
          (lambda ()
            (superword-mode 1)
            (setq
             python-shell-interpreter "ipython"
             python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
             python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
          )

;; jedi setup
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'jedi-mode-hook (lambda () (setq jedi:complete-on-dot t)))

;;; ack-python.el ends here
