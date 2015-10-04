;;; interactive.el --- Interactive commands

;; Copyright (C) 2014  Alberto Donato

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

(require 'cl)

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
          (remove-if
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
  "Indent current buffer."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (indent-region b e)))

(defun beautify-json ()
  "Indent JSON data."
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e "json-indent" (current-buffer) t)))

(defun python-output-requote (start end)
  "Double-quote python region between START and END and remove unicode prefix."
  (interactive "r")

  ;; remove unicode prefix from double-quoted strings
  (replace-regexp "u?\"\\(\\([^\\]\\|\\\\\"\\)*?\\)\"" "\"\\1\"" nil start end)

  (goto-char start)
  (while (re-search-forward "u?\'\\(\\([^\\]\\|\\\\\'\\)*?\\)\'" end t)
    (let ((start-position (match-beginning 0)) (end-position (match-end 0))
          (matched (buffer-substring (match-beginning 1) (match-end 1)))
          (main-buffer (current-buffer)))
      (with-temp-buffer
        (insert matched)
        ;; escape double quotes
        (replace-regexp "\"" "\\\\\"" nil (point-min) (point-max))
        ;; unescape single quotes
        (replace-regexp "\\\\'" "\'" nil (point-min) (point-max))
        (let ((replacement (buffer-string)))
          (set-buffer main-buffer)
          (delete-region start-position end-position)
          (goto-char start-position)
          (insert "\"" replacement "\""))))))

;;; interactive.el ends here
