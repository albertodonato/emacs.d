;;; ack-editing.el -- Editing settings

;;; Commentary:
;;;   General editing configuration.

;;; Code:

(use-package expand-region
  :bind (("C-+" . 'er/expand-region)))

(use-package files
  :ensure nil
  :hook ((after-save . (lambda ()
                         "Make file executable if it starts with a shebang."
                         (and (save-excursion
                                (save-restriction
                                  (widen)
                                  (goto-char (point-min))
                                  (save-match-data (looking-at "^#!"))))
                              (not (file-executable-p buffer-file-name))
                              (shell-command (concat "chmod +x " buffer-file-name))
                              (message (concat "Saved as script: " buffer-file-name))))))
  :custom
  (require-final-newline t))

(use-package smartparens
  :init (smartparens-global-mode t)
  :custom
  (sp-ignore-modes-list '(minibuffer-inactive-mode erc-mode fundametal-mode)))

(use-package smartparens-config
  :ensure nil
  :after (smartparens))

(use-package multiple-cursors
  :bind (("C-c C->" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<". mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this-dwim)))

(provide 'ack-editing)
;;; ack-editing.el ends here
