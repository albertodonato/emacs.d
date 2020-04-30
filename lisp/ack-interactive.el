;;; ack-interactive.el --- Interactive commands

;;; Commentary:
;;;   Interactive commands.

;;; Code:

(defun ack/sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

(defun ack/kill-all-file-buffers ()
  "Kill all buffers that visit a file."
  (interactive)
  (mapc
   (lambda (buf) (when (buffer-file-name buf) (kill-buffer buf)))
   (buffer-list)))

(defun ack/shell-command-on-region-replace (command)
  "Execute COMMAND with region as input and replace it with its output."
  (interactive "sShell command: ")
  (shell-command-on-region (region-beginning) (region-end) command t t))

(defun ack/yaml-to-python ()
  "Convert region from YAML to Python."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end)
   "python3 -c 'import sys; import yaml; import pprint; pprint.pprint(yaml.safe_load(sys.stdin))'"
   t t))

(defun ack/get-faces-at-point ()
  "Print out font faces at point."
  (interactive)
  (message "Faces at point: %s"
           (remq nil
                 (list
                  (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face)
                  (plist-get (text-properties-at (point)) 'face)))))

(defun ack/toggle-window-dedicated ()
  "Toggle dedicated status for current window."
  (interactive)
  (let ((window (selected-window)))
    (set-window-dedicated-p window (not (window-dedicated-p window)))
    (message "Toggle window dedicated to %S" (window-dedicated-p window))))

(provide 'ack-interactive)
;;; ack-interactive.el ends here
