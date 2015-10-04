;;; ack-theme-el --- A emacs theme with dark background

;; Copyright (C) 2014  Alberto Donato

;; Author: Alberto Donato <alberto.donato@gmail.com>
;; Keywords: theme

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

;; My own emacs theme.

;;; Code:

(deftheme ack "Created by Alberto Donato.")

(custom-theme-set-faces
 'ack
 '(ack-match ((t (:foreground "Orange" :background "Black"))))

 '(diff-added ((t (:foreground "Green" :background "Black"))))
 '(diff-removed ((t (:foreground "Red" :background "Black"))))
 '(diff-changed ((t (:foreground "LightBlue" :background "Black"))))
 '(diff-header ((t (:foreground "Blue" :background "Black"))))
 '(diff-hunk-header ((t (:foreground "LightBlue" :background "Black"))))
 '(diff-file-header ((t (:foreground "Yellow" :background "Black"))))

 '(flymake-errline ((t (:foreground "White" :background "DarkRed"))))
 '(flymake-warnline ((t (:foreground "White" :background "DarkBlue"))))
 '(flycheck-error ((t (:foreground "White" :background "DarkRed"))))
 '(flycheck-warning ((t (:foreground "White" :background "DarkBlue"))))
 '(flycheck-fringe-error ((t (:foreground "White" :background "DarkRed"))))
 '(flycheck-fringe-warning ((t (:foreground "White" :background "DarkBlue"))))

 '(ido-first-match ((t (:foreground "PaleGreen"))))
 '(ido-only-match ((t (:foreground "PaleGreen"))))
 '(isearch ((t (:background "ForestGreen"))))
 '(cursor ((t (:foreground "DarkGray" :background "White"))))
 '(region ((t (:background "Gray40"))))
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White"
                         :inverse-video nil :box nil :strike-through nil :overline nil
                         :underline nil :slant normal :weight normal :height 102 :width normal
                         :foundry "unknown" :family "Ubuntu Mono"))))
 '(hl-line ((t (:background "Gray20"))))
 '(tabbar-default ((t (:inherit t :height 0.9 :foreground "White" :background "DarkGray"))))
 '(eshell-prompt ((t (:foreground "Green" :weight normal))))

 '(erc-current-nick-face ((t (:foreground "Yellow"))))
 '(erc-prompt-face ((t (:background "Black" :foreground "DarkGray"))))
 '(erc-timestamp-face ((t (:foreground "DimGray"))))

 '(rcirc-my-nick ((t (:foreground "Yellow"))))
 '(rcirc-timestamp ((t (:foreground "DimGray"))))
 '(rcirc-track-keyword ((t (:foreground "LightGreen"))))
 '(rcirc-prompt ((t (:foreground "DarkGray"))))
 )

(provide-theme 'ack)

;;; ack-theme.el ends here
