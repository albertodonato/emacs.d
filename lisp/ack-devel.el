;; ack-devel.el --- Development related modes

;;; Commentary:
;;;   Modes related to development.

;;; Code:

(use-package company
  :init (global-company-mode t)
  :bind (:map company-active-map
              ("RET" . company-complete-selection)
              ("<return>" . company-complete-selection)
              ("TAB" . company-complete-common-or-cycle)
              ("<tab>" . company-complete-common-or-cycle))
  :custom
  (company-idle-delay 0.5)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-tooltip-align-annotations t))

(use-package command-log-mode)

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error))

(use-package deadgrep
  :bind (("C-c C-S-a" . deadgrep)))

(use-package transient
  :custom
  (transient-levels-file (ack/in-cache-dir "transient/levels.el"))
  (transient-values-file (ack/in-cache-dir "transient/values.el"))
  (transient-history-file (ack/in-cache-dir "transient/history.el")))

(use-package magit-ido)

(use-package magit
  :after (magit-ido)
  :custom
  (magit-completing-read-function 'magit-ido-completing-read)
  :config
  (progn
    (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-modules nil t)))

(use-package projectile
  :init (projectile-mode t)
  :custom
  (projectile-known-projects-file (ack/in-cache-dir "projectile-bookmarks.eld")))

(use-package restclient)

(use-package scratch
  :bind (("C-c s" . scratch)))

(use-package vterm
  :bind (("C-c v" . vterm-other-window)))

(use-package whitespace
  :bind (("C-c w" . whitespace-mode)
         ("C-M-`" . whitespace-cleanup)))

(use-package webpaste
  :bind (("C-c C-p C-b" . webpaste-paste-buffer)
         ("C-c C-p C-r" . webpaste-paste-region)
         ("C-c C-p C-p" . webpaste-paste-buffer-or-region))
  :custom
  (webpaste-provider-priority '("paste.ubuntu.com")))

(provide 'ack-devel)
;;; ack-devel.el ends here
