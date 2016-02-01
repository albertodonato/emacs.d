;;; ack-interactive.el --- Interactive commands

;;; Commentary:
;;;   Interactive commands.

;;; Code:

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

(defun kill-all-file-buffers ()
  "Kill all buffers that visit a file."
  (interactive)
  (mapc
   (lambda (buf) (when (buffer-file-name buf) (kill-buffer buf)))
   (buffer-list)))

(defun current-buffer-path ()
  "Print path for file in current buffer."
  (interactive)
  (message buffer-file-name))

(defun indent-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max))))

(defun shell-command-on-region-replace (command)
  "Execute COMMAND with region as input and replace it with output."
  (interactive "sShell command: ")
  (shell-command-on-region (region-beginning) (region-end) command t t))

(defun python-insert-pdb-statement ()
  "Insert pdb statement on the line at point."
  (interactive)
    (insert-text-and-reindent "import pdb; pdb.set_trace()"))

(defun insert-text-and-reindent (text)
  "Insert TEXT on a new line at point and reindent."
  (interactive)
  (let ((beg (line-beginning-position)))
    (beginning-of-line)
    (insert (concat text "\n"))
    (indent-region beg (line-end-position))))

(provide 'ack-interactive)
;;; ack-interactive.el ends here
