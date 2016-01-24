;;; ack-theme-el --- An Emacs theme with dark background.

;;; Commentary:
;;;   My personal Emacs theme.

;;; Code:

(deftheme ack "Dark background theme.")

(custom-theme-set-faces
 'ack
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White"
                         :inverse-video nil :box nil :strike-through nil :overline nil
                         :underline nil :slant normal :weight normal :height 102 :width normal
                         :foundry "unknown" :family "Ubuntu Mono"))))
 '(cursor ((t (:foreground "DarkGray" :background "White"))))
 '(region ((t (:background "Gray40"))))
 '(hl-line ((t (:background "Gray20"))))
 '(font-lock-warning-face ((t (:foreground "Yellow"))))
 '(compilation-error ((t (:foreground "Red"))))
 '(compilation-line-number ((t (:foreground "Yellow"))))
 '(compilation-column-number ((t (:foreground "Red"))))
 '(match ((t (:foreground "Gray40" :background "Orange" :weight bold))))
 '(isearch ((t (:background "ForestGreen"))))
 '(ido-first-match ((t (:foreground "PaleGreen"))))
 '(ido-only-match ((t (:foreground "PaleGreen"))))
 '(eshell-prompt ((t (:foreground "Green" :weight normal))))
 '(success ((t (:foreground "Green"))))

 '(diff-added ((t (:foreground "Green" :background "Black"))))
 '(diff-removed ((t (:foreground "Red" :background "Black"))))
 '(diff-changed ((t (:foreground "LightBlue" :background "Black"))))
 '(diff-header ((t (:foreground "LightSkyBlue" :background "Black"))))
 '(diff-hunk-header ((t (:foreground "LightSkyBlue" :background "Black"))))
 '(diff-file-header ((t (:foreground "Yellow" :background "Black"))))

 '(flymake-errline ((t (:foreground "White" :background "DarkRed"))))
 '(flymake-warnline ((t (:foreground "White" :background "DarkBlue"))))

 '(flycheck-error ((t (:foreground "White" :background "DarkRed"))))
 '(flycheck-warning ((t (:foreground "White" :background "DarkBlue"))))
 '(flycheck-fringe-error ((t (:foreground "White" :background "DarkRed"))))
 '(flycheck-fringe-warning ((t (:foreground "White" :background "DarkBlue"))))

 '(jedi:highlight-function-argument ((t (:foreground "PaleGreen"))))

 '(erc-current-nick-face ((t (:foreground "Yellow"))))
 '(erc-input-face ((t (:foreground "Gray"))))
 '(erc-my-nick-face ((t (:foreground "LightGray"))))
 '(erc-prompt-face ((t (:background "Black" :foreground "DarkGray"))))
 '(erc-timestamp-face ((t (:foreground "DimGray"))))
 )

(provide-theme 'ack)

;;; ack-theme.el ends here
