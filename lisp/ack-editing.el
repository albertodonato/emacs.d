;;; ack-editing.el -- Editing settings

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

;; General editing configuration.

;;; Code:

(require 'uniquify)
(require 'iso-transl) ;; For dead keys
(require 'dired-x)
(require 'multiple-cursors)

(setq scroll-conservatively 5)
(setq fill-column 79)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default require-final-newline t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; use UTF-8 when pasting
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq browse-url-browser-function 'browse-url-default-browser)

;; ido-mode
(require 'ido)
(add-hook 'ido-mode-hook
          (progn
            (ido-mode t)
            (ido-everywhere t)
            (setq ido-enable-flex-matching t
                  ;; Display ido results vertically, rather than horizontally
                  ido-decorations '("\n " "" "\n " "\n   ..."
                                    "[" "]" " [No match]" " [Matched]"
                                    " [Not readable]" " [Too big]" " [Confirm]"))
            (defun ido-disable-line-trucation ()
              (set (make-local-variable 'truncate-lines) nil))
            (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)))

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(add-hook 'after-init-hook 'global-auto-complete-mode)
(ac-config-default)
(setq ac-comphist-file "~/.emacs-saves/auto-complete.dat")

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; smartparens-mode
(require 'smartparens)
(require 'smartparens-config)
(add-hook 'after-init-hook 'smartparens-global-mode)
(show-smartparens-global-mode t)
(setq sp-ignore-modes-list '(minibuffer-inactive-mode erc-mode fundametal-mode))

(provide 'ack-editing)

;;; ack-editing.el ends here
