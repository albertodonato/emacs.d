;;; ack-editing.el -- Editing settings

;;; Commentary:
;;;   General editing configuration.

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
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; use UTF-8 when pasting
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(setq browse-url-browser-function 'browse-url-default-browser)

(require 'ido)
(require 'flx-ido)
(add-hook 'ido-mode-hook
          (progn
            (ido-mode t)
            (ido-everywhere t)
            (flx-ido-mode 1)
            (setq ido-enable-flex-matching t
                  flx-ido-use-faces nil
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

;; (require 's)
;; (defadvice sp-show--pair-function (after sp-show--pair-function-offscreen activate)
;;   "If the matching paren is offscreen, show the matching line in the echo area."
;;   (interactive)
;;   (let ((vis-buf (save-excursion
;;                    (cons
;;                     (progn (move-to-window-line 0) (point))
;;                     (progn (move-to-window-line -1) (line-end-position)))))
;;         (matching-sexp (if (and (sp-get (sp-get-sexp nil) :beg)
;;                                 (= (point) (sp-get (sp-get-sexp nil) :beg)))
;;                            (cons (sp-get (sp-get-sexp nil) :beg)
;;                                  (sp-get (sp-get-sexp nil) :end))
;;                          (if (and (not (= (point) (point-min)))
;;                                   (sp-get (sp-get-sexp t) :end)
;;                                   (= (point) (sp-get (sp-get-sexp t) :end)))
;;                              (cons (sp-get (sp-get-sexp t) :beg)
;;                                    (sp-get (sp-get-sexp t) :end))
;;                            nil))))
;;     (when matching-sexp
;;       (if (> (car vis-buf) (car matching-sexp))
;;           ;; opening delim is offscreen
;;           (message "Matches %s"
;;                    (s-trim
;;                     (save-excursion
;;                       (goto-char (car matching-sexp))
;;                       (thing-at-point 'line))))
;;         (if (< (cdr vis-buf) (cdr matching-sexp))
;;             ;; closing delim is offscreen
;;             (message "Matches %s"
;;                      (s-trim
;;                       (save-excursion
;;                         (goto-char (cdr matching-sexp))
;;                         (thing-at-point 'line)))))))))

(provide 'ack-editing)

;;; ack-editing.el ends here
