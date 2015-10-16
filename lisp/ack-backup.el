;;; ack-backup.el --- Backup and autosave

;; Copyright (C) 2015-  Alberto Donato

;; Author: Alberto Donato <alberto.donato@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Backup and austosave configuration

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
