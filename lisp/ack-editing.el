;;; ack-editing.el -- Editing settings

;;; Commentary:
;;;   General editing configuration.

;;; Code:

(require 'uniquify)
(require 'iso-transl) ;; For dead keys
(require 'dired-x)
(require 'dired-subtree)
(require 'expand-region)
(require 'hippie-exp)

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

;; make the file executable on save if it has a shebang
(add-hook 'after-save-hook
        #'(lambda ()
            (and (save-excursion
               (save-restriction
                 (widen)
                 (goto-char (point-min))
                 (save-match-data (looking-at "^#!"))))
                 (not (file-executable-p buffer-file-name))
                 (shell-command (concat "chmod +x " buffer-file-name))
                 (message (concat "Saved as script: " buffer-file-name)))))

(require 'ido)
(require 'flx-ido)
(add-hook 'ido-mode-hook
          (progn
            (ido-mode t)
            (ido-everywhere t)
            (flx-ido-mode 1)
            (setq ido-enable-flex-matching t
                  flx-ido-use-faces nil
                  ido-save-directory-list-file (file-path-in-cache-dir "ido.last")
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

(require 'smex)
(smex-initialize)
(setq smex-save-file (file-path-in-cache-dir "smex-items"))

(require 'multiple-cursors)
(setq mc/list-file (file-path-in-cache-dir "mc-list.el"))

(provide 'ack-editing)
;;; ack-editing.el ends here
