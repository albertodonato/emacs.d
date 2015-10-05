;;; editing.el -- Editing settings

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

;; General editing configuration.

;;; Code:

(setq scroll-conservatively 5)
(setq fill-column 79)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default require-final-newline t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; use UTF-8 when pasting
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      (quote (read-only
              t point-entered minibuffer-avoid-prompt
              face minibuffer-prompt)))

(setq browse-url-browser-function 'browse-url-default-browser)

;;; editing.el ends here
