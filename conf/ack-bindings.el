;;; ack-bindings.el -- Custom key bindings

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

;; Additional key bindings.

;;; Code:

(require 'dired)
(require 'go-mode)

(global-set-key (kbd "M-p") 'current-buffer-path)
(global-set-key (kbd "C-M-`") 'open-emacs-dir)
(global-set-key (kbd "C-c k") 'kill-all-buffers)
(global-set-key (kbd "C-c C-j") 'json-pretty-print)
(global-set-key (kbd "C-c C-S-a") 'ack)
(global-set-key (kbd "C-M-|") 'indent-whole-buffer)
(global-set-key (kbd "C-c M-r") 'revert-buffer)
(global-set-key (kbd "C-c s") 'string-insert-rectangle)

(global-set-key (kbd "C-c M-e") 'eshell)
(global-set-key (kbd "C-c M-t") (lambda () (interactive) (ansi-term "/bin/bash")))

(global-set-key (kbd "C-c n") 'linum-mode)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim)

(add-hook 'golang-load-hook
          (define-key go-mode-map (kbd "M-g f") 'gofmt))

(add-hook 'dired-load-hook
          (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(provide 'ack-bindings)

;;; ack-bindings.el ends here
