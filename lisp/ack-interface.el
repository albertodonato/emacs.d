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

;; better unique naming for buffers
(use-package uniquify
  :ensure nil)

;; support dead keys
(use-package iso-transl
  :ensure nil)

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-c C-q" . wdired-change-to-wdired-mode)
              ("C-<right>" . dired-subtree-insert)
              ("C-<left>" . dired-subtree-remove)))

(use-package dired-x
  :ensure nil
  :after (dired))

(use-package dired-subtree
  :ensure nil
  :after (dired))

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :config
  (setq doom-modeline-height 12
        doom-modeline-bar-width 10
        doom-modeline-buffer-encoding nil
        doom-modeline-icon t
        doom-modeline-env-python-executable "python3"))

(use-package ido
  :init (ido-mode t)
  :config
  (setq ido-everywhere t
        ido-virtual-buffers t
        ido-use-faces t
        ido-enable-flex-matching t
        ido-save-directory-list-file (ack/in-cache-dir "ido.last")
        ido-default-buffer-method 'selected-window
        ;; Display ido results vertically, rather than horizontally
        ido-decorations '("\n " "" "\n " "\n   ..."
                          "[" "]" " [No match]" " [Matched]"
                          " [Not readable]" " [Too big]" " [Confirm]")))

(use-package ido-completing-read+
  :after (ido)
  :init  (ido-ubiquitous-mode t)
  :config
  (setq ido-cr+-max-items 50000))

(use-package flx-ido
  :after (ido)
  :init (flx-ido-mode t)
  :config
  (setq flx-ido-use-faces nil))

(use-package crm-custom  ;; so that ido wraps complete-read-multiple too
  :init (crm-custom-mode t))

(use-package hl-line
  :ensure nil
  :init (global-hl-line-mode t))

(use-package linum-mode
  :ensure nil
  :bind (("C-c n" . linum-mode)))

(use-package paren
  :ensure nil
  :init (show-paren-mode t))

(use-package simple
  :ensure nil
  :init (column-number-mode t))

(use-package smex
  :bind ("M-x" . smex)
  :init (smex-initialize)
  :config
  (setq smex-save-file (ack/in-cache-dir "smex-items")))

(use-package winner
  :ensure nil
  :init (winner-mode))

(use-package desktop
  :ensure nil
  :init (desktop-save-mode)
  :config
  (setq desktop-auto-save-timeout 300
        desktop-load-locked-desktop t
        desktop-restore-eager 20))

(use-package which-func
  :ensure nil
  :init (which-function-mode t))

(use-package lxd-tramp)

(provide 'ack-interface)
;;; ack-interface.el ends here
