;;; ack-bindings.el -- Custom key bindings

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

;; Additional key bindings.

;;; Code:

(global-set-key (kbd "M-p") 'current-buffer-path)
(global-set-key (kbd "C-c C-S-a") 'ack)
(global-set-key (kbd "C-M-|") 'indent-whole-buffer)
(global-set-key (kbd "C-c M-r") 'revert-buffer)
(global-set-key (kbd "C-c s") 'string-insert-rectangle)

(global-set-key (kbd "C-c M-e") 'eshell)
(global-set-key (kbd "C-c M-t") (lambda () (interactive) (ansi-term "/bin/bash")))

(global-set-key (kbd "C-c n") 'linum-mode)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(with-eval-after-load 'multiple-cursors
  (global-set-key (kbd "C-c C->") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this-dwim))

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "M-g f") 'gofmt))
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))
(with-eval-after-load 'yaml-mode
  (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c x") 'jedi-direx:pop-to-buffer))

(provide 'ack-bindings)

;;; ack-bindings.el ends here
