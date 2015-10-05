;;; init.el --- Emacs init file

;; Copyright (C) 2015  Alberto Donato

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

;; Emacs default config file.
;; This is the entry point to load other config files.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/conf")
(add-to-list 'load-path "~/.emacs.d/lisp")

(load "ack-packages.el")


;; autosaves and backup
(setq auto-save-file-name-transforms '((".*" "~/.emacs.saves/autosaves/\\1" t)))
(setq auto-save-list-file-prefix "~/.emacs.saves/autosaves/saves-")
(setq backup-directory-alist '((".*" . "~/.emacs.saves/backups/")))
(setq session-save-file "~/.emacs.saves/sessions/.session")

;; themes
(setq custom-theme-directory "~/.emacs.d/themes")
(setq custom-safe-themes t)

;; custom
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; load other config files
(load "appearance")
(load "editing")
(load "modes")
(load "interactive")
(load "bindings")
(load-file "~/Documents/private/keys/irc.el")
(load "irc")

(provide 'init)

;;; init.el ends here
