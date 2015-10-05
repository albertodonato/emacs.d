;; modes.el --- Modes configuration

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

;; Modes loading and configuration.

;;; Code:

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode t)
(column-number-mode t)
(which-function-mode t)
(show-paren-mode t)
(winner-mode t)

;; For dead keys
(require 'iso-transl)

(require 'dired-x)

;; tabbar-mode
(when (load "tabbar" t)
  (tabbar-mode t))

;; IDO mode
(when (load "ido" t)
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t)
  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations '("\n " "" "\n " "\n   ..."
                          "[" "]" " [No match]" " [Matched]"
                          " [Not readable]" " [Too big]" " [Confirm]"))
  (defun ido-disable-line-trucation () 
    (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation))

;; full-ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
(setq ack-executable "/usr/bin/ack-grep")
(setq ack-use-environment nil)

;; auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)
(setq ac-comphist-file "~/.emacs.saves/auto-complete.dat")
(setq ac-dictionary-directories '("/usr/share/auto-complete/dict"))

;; multiple-cursors
(require 'multiple-cursors)

;; jedi
(when (load "jedi" t)
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

;; flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; vc-mode
(setq vc-diff-switches "-u")

;; python-mode
(when (load "python" t)
  (add-hook 'python-mode-hook '(lambda () (superword-mode 1)))
  (setq
   python-shell-interpreter "ipython"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;; smartparens-mode
(require 'smartparens)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(require 'smartparens-config)
(setq sp-ignore-modes-list '(minibuffer-inactive-mode erc-mode fundametal-mode))

;; twisted tac files
(when (load "python" t)
  (setq python-check-command "pycheck")
  (add-to-list 'auto-mode-alist '("\\.tac$" . python-mode)))

;; Zope files
(add-to-list 'auto-mode-alist '("\\.zcml$" . xml-mode))

(add-to-list 'auto-mode-alist '("\\.pt$" . html-mode))

;; yaml-mode
(when (load "yaml-mode" t)
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

;; js-mode
(when (load "js" t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . js-mode)))

;; whitespace-mode
(when (load "whitespace" t)
  (autoload 'whitespace-toggle-options "whitespace" t))

;;; modes.el ends here
