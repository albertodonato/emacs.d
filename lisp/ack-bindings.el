;;; ack-bindings.el -- Custom key bindings

;;; Commentary:
;;;   Additional key bindings.

;;; Code:

(global-set-key (kbd "C-`") (lambda () (interactive) (dired user-emacs-directory)))
;; print buffer name
(global-set-key (kbd "M-p") (lambda () (interactive) (message buffer-file-name)))

(global-set-key (kbd "C-c M-r") 'revert-buffer)

(global-set-key (kbd "C-x M-k") 'kill-matching-buffers)

(global-set-key (kbd "C-M-`") 'whitespace-cleanup)
(global-set-key (kbd "C-c s") 'string-insert-rectangle)
(global-set-key (kbd "C-c C-!") 'ack/shell-command-on-region-replace)
;; indent whole buffer
(global-set-key (kbd "C-M-|") (lambda () (interactive)
                                (save-excursion
                                  (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-c M-e") 'eshell)
(global-set-key (kbd "C-c M-t") (lambda () (interactive) (ansi-term "/bin/bash")))

(global-set-key (kbd "C-c n") 'linum-mode)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(global-set-key (kbd "C-c C-S-a") 'ag-project-regexp)
(global-set-key (kbd "C-c C-S-f") 'ag-project-files)

(global-set-key (kbd "M-x") 'smex)

(global-set-key (kbd "C-+") 'er/expand-region)

(global-set-key (kbd "C-c C->") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)

(with-eval-after-load 'ag-mode
  (define-key ag-mode-map (kbd "<C-return>")
    'ack/ag-compile-goto-error-and-kill-search-buffer))

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "M-g f") 'gofmt))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(with-eval-after-load 'dired-subtree
  (define-key dired-mode-map (kbd "C-<right>") 'dired-subtree-insert)
  (define-key dired-mode-map (kbd "C-<left>") 'dired-subtree-remove))

(with-eval-after-load 'yaml-mode
  (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c x") 'jedi-direx:pop-to-buffer))

(provide 'ack-bindings)
;;; ack-bindings.el ends here
