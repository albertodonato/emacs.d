;;; ack-interactive.el --- Interactive commands

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

;; Interactive commands.

;;; Code:

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (let ((keep-buffers '("*scratch*" "*Messages*")))
    (mapc 'kill-buffer
          (cl-remove-if
           (lambda (buf) (member (buffer-name buf) keep-buffers))
           (buffer-list)))))

(defun current-buffer-path ()
  "Print path for file in current buffer."
  (interactive)
  (message buffer-file-name))

(defun open-emacs-dir ()
  "Open Emacs config dir."
  (interactive)
  (dired "~/.emacs.d"))

(defun indent-whole-buffer ()
  "Indent the current buffer."
  (interactive)
  (save-excursion (indent-region (point-min) (point-max))))

(provide 'ack-interactive)

;;; ack-interactive.el ends here
