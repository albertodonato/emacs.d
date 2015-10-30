;;; ack-editing.el -- Editing settings

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

;; General editing configuration.

;;; Code:

(require 'uniquify)
(require 'iso-transl) ;; For dead keys
(require 'dired-x)
(require 'multiple-cursors)

(setq-default scroll-conservatively 5
              fill-column 79
              tab-width 4
              indent-tabs-mode nil
              require-final-newline t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; use UTF-8 when pasting
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq browse-url-browser-function 'browse-url-default-browser)

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

(require 'auto-complete)
(require 'auto-complete-config)
(add-hook 'after-init-hook 'global-auto-complete-mode)
(ac-config-default)
(setq ac-comphist-file "~/.emacs-saves/auto-complete.dat")

(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

(require 'smartparens)
(require 'smartparens-config)
(add-hook 'after-init-hook 'smartparens-global-mode)
(show-smartparens-global-mode t)
(setq sp-ignore-modes-list '(minibuffer-inactive-mode erc-mode fundametal-mode))

(require 's)
(defadvice sp-show--pair-function (after sp-show--pair-function-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the echo area."
  (interactive)
  (let ((vis-buf (save-excursion
                   (cons
                    (progn (move-to-window-line 0) (point))
                    (progn (move-to-window-line -1) (line-end-position)))))
        (matching-sexp (if (and (sp-get (sp-get-sexp nil) :beg)
                                (= (point) (sp-get (sp-get-sexp nil) :beg)))
                           (cons (sp-get (sp-get-sexp nil) :beg)
                                 (sp-get (sp-get-sexp nil) :end))
                         (if (and (not (= (point) (point-min)))
                                  (sp-get (sp-get-sexp t) :end)
                                  (= (point) (sp-get (sp-get-sexp t) :end)))
                             (cons (sp-get (sp-get-sexp t) :beg)
                                   (sp-get (sp-get-sexp t) :end))
                           nil))))
    (when matching-sexp
      (if (> (car vis-buf) (car matching-sexp))
          ;; opening delim is offscreen
          (message "Matches %s"
                   (s-trim
                    (save-excursion
                      (goto-char (car matching-sexp))
                      (thing-at-point 'line))))
        (if (< (cdr vis-buf) (cdr matching-sexp))
            ;; closing delim is offscreen
            (message "Matches %s"
                     (s-trim
                      (save-excursion
                        (goto-char (cdr matching-sexp))
                        (thing-at-point 'line)))))))))

(provide 'ack-editing)

;;; ack-editing.el ends here
