;; ack-devel.el --- Development related modes

;;; Commentary:
;;;   Modes related to development.

;;; Code:

(use-package ag
  :bind (("C-c C-S-a" . ag-project-regexp)
         ("C-c C-S-f" . ag-project-files)
         :map ag-mode-map
         ("<C-return>" . (lambda ()
                           "Jump to location and close the buffer."
                           (interactive)
                           (let ((buf (current-buffer)))
                             (compile-goto-error)
                             (kill-buffer buf)))))
  :config
  (setq ag-highlight-search t
        ag-group-matches t
        ag-context-lines 3))

(use-package company
  :hook ((after-init . global-company-mode))
  :bind (:map company-active-map
              ("RET" . nil)
              ("TAB" . company-complete-selection)
              ("<right>" . company-complete-common))
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-require-match nil
        company-tooltip-align-annotations t))

(use-package command-log-mode)

(use-package compile
  :ensure nil
  :config (setq compilation-scroll-output 'first-error))

(use-package flycheck
  :hook ((after-init . global-flycheck-mode))
  :config
  (setq flycheck-checker-error-threshold 2000
        flycheck-python-pycompile-executable "python3 -I"))

(use-package linum-mode
  :ensure nil
  :bind (("C-c n" . linum-mode)))

(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-c S-g" . magit-refresh-all))
  :init
  (use-package transient
    :config
    (setq transient-levels-file (ack/in-cache-dir "transient/levels.el")
          transient-values-file (ack/in-cache-dir "transient/values.el")
          transient-history-file (ack/in-cache-dir "transient/history.el")))
  :config
  (magit-add-section-hook 'magit-status-sections-hook 'magit-insert-modules nil t))

(use-package projectile
  :config
  (setq projectile-known-projects-file (ack/in-cache-dir "projectile-bookmarks.eld"))
  (projectile-mode))

(use-package restclient)

(use-package scratch
  :bind (("C-c s" . scratch)))

(use-package whitespace
  :bind (("C-c w" . whitespace-mode)
         ("C-M-`" . whitespace-cleanup)))

(provide 'ack-devel)
;;; ack-devel.el ends here
