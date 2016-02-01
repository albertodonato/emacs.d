;;; ack-backup.el --- Backup and autosave

;;; Commentary:
;;;   Backup and austosave configuration.

;;; Code:

(setq auto-save-file-name-transforms '((".*" "~/.emacs-saves/autosaves/\\1" t))
      auto-save-list-file-prefix "~/.emacs-saves/autosaves/saves-")
(setq backup-directory-alist '((".*" . "~/.emacs-saves/backups/"))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 2
      version-control t)

(provide 'ack-backup)
;;; ack-backup.el ends here
