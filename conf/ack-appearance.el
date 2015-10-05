;;; ack-appearance.el --- Appearance settings

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

;; Appearance and themes settings.

;;; Code:

(setq inhibit-startup-message t)
(setq initial-frame-alist '((fullscreen . maximized)))

;; display Emacs version and buffer name in window title
(setq frame-title-format
      '("Emacs " emacs-version ": "
        (:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))

(setq custom-theme-directory "~/.emacs.d/themes")
(setq custom-safe-themes t)

(provide 'ack-appearance)

;;; ack-appearance.el ends here
