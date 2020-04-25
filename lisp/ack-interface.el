;;; ack-interface.el -- Interface configuration

;;; Commentary:
;;;   General configuration for interface and intaractions.

;;; Code:

(setq-default scroll-conservatively 5
              fill-column 79
              tab-width 4
              indent-tabs-mode nil)
(setq browse-url-browser-function 'browse-url-default-browser
      ;; use UTF-8 when pasting
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; don't let the cursor go into minibuffer prompt
      minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

;; use y/n to answer questions
(fset 'yes-or-no-p 'y-or-n-p)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page   'disabled nil)
(put 'narrow-to-region 'disabled nil)

(use-package uniquify
  :ensure nil)

 ;; dead keys
(use-package iso-transl
  :ensure nil)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-q" . wdired-change-to-wdired-mode)
              ("C-<right>" . dired-subtree-insert)
              ("C-<left>" . dired-subtree-remove))
  :config
  (use-package dired-x
    :ensure nil)
  (use-package dired-subtree
    :ensure nil))

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t
        ido-save-directory-list-file (ack/in-cache-dir "ido.last")
        ;; Display ido results vertically, rather than horizontally
        ido-decorations '("\n " "" "\n " "\n   ..."
                          "[" "]" " [No match]" " [Matched]"
                          " [Not readable]" " [Too big]" " [Confirm]")))

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq flx-ido-use-faces nil))

(use-package smex
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (ack/in-cache-dir "smex-items"))
  (smex-initialize))

(provide 'ack-interface)
;;; ack-interface.el ends here
