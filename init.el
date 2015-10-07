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

(add-to-list 'load-path "~/.emacs.d/conf")
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'ack-custom) ;; for package-selected-packages

;; packages configuration
(setq package-user-dir "~/.emacs-packages")
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-install-selected-packages)

(require 'ack-backup)
(require 'ack-interactive)
(require 'ack-editing)
(require 'ack-modes)
(require 'ack-automode)
(require 'ack-bindings)
(load-file "~/Documents/private/keys/irc.el")
(require 'ack-irc)
(require 'ack-appearance)

(provide 'init)

;;; init.el ends here
