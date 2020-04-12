;;; ack-bindings.el -- Custom key bindings

;;; Commentary:
;;;   Additional key bindings.

;;; Code:

;; print buffer name
(global-set-key (kbd "M-p") (lambda () (interactive) (message buffer-file-name)))

(global-set-key (kbd "C-c M-r") 'revert-buffer)

(global-set-key (kbd "C-x M-k") 'kill-matching-buffers)

(global-set-key (kbd "C-c s") 'string-insert-rectangle)
(global-set-key (kbd "C-c C-!") 'ack/shell-command-on-region-replace)
;; indent whole buffer
(global-set-key (kbd "C-M-|") (lambda () (interactive)
                                (save-excursion
                                  (indent-region (point-min) (point-max)))))

(global-set-key (kbd "C-c M-e") 'eshell)
(global-set-key (kbd "C-c M-t") 'shell)

(global-set-key (kbd "C-c t") 'ack/toggle-window-dedicated)
;; jump to minibuffer
(global-set-key (kbd "C-c m") (lambda () (interactive)
                                (when (active-minibuffer-window)
                                  (select-window (active-minibuffer-window)))))

(provide 'ack-bindings)
;;; ack-bindings.el ends here
