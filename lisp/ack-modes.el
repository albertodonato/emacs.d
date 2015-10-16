;; ack-modes.el --- Modes configuration

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

;; Modes loading and configuration.

;;; Code:

(require 'secrets)
(require 'ox-reveal)

;; full-ack
(require 'full-ack)
(setq ack-executable "/usr/bin/ack-grep"
      ack-use-environment nil)

(provide 'ack-modes)

;;; ack-modes.el ends here
