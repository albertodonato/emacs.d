;;; ack-theme-el --- An Emacs theme with dark background.

;;; Commentary:
;;;   My personal Emacs theme.

;;; Code:

(deftheme ack "Dark background theme.")

(custom-theme-set-faces
 'ack
 '(default ((t (:background "Black" :foreground "White"))))

 '(fringe ((t (:background "Gray10"))))
 '(cursor ((t (:foreground "DarkGray" :background "White"))))
 '(region ((t (:background "Gray40"))))
 '(hl-line ((t (:background "Gray20"))))
 '(font-lock-warning-face ((t (:foreground "Yellow"))))
 '(eshell-prompt ((t (:foreground "Green" :weight normal))))
 '(success ((t (:foreground "Green"))))

 '(compilation-error ((t (:foreground "Red"))))
 '(compilation-line-number ((t (:foreground "Yellow"))))
 '(compilation-column-number ((t (:foreground "Red"))))

 '(match ((t (:foreground "Yellow" :underline "Yellow" :weight bold))))
 '(isearch ((t (:background "ForestGreen"))))

 '(ido-first-match ((t (:foreground "PaleGreen"))))
 '(ido-only-match ((t (:foreground "PaleGreen"))))

 '(diff-added ((t (:inherit 'diff-context :foreground "Green"))))
 '(diff-removed ((t (:inherit 'diff-context :foreground "Red"))))
 '(diff-changed ((t (:inherit 'diff-context :foreground "LightBlue"))))
 '(diff-header ((t (:inherit 'diff-context :foreground "LightSkyBlue"))))
 '(diff-hunk-header ((t (:inherit 'diff-context :foreground "LightSkyBlue"))))
 '(diff-file-header ((t (:inherit 'diff-context :foreground "Yellow"))))

 '(flymake-errline ((t (:underline (:color "Red" :style wave)))))
 '(flymake-warnline ((t (:underline (:color "DeepSkyBlue" :style wave)))))

 '(flycheck-error ((t (:underline (:color "Red" :style wave)))))
 '(flycheck-warning ((t (:underline (:color "DeepSkyBlue" :style wave)))))
 '(flycheck-fringe-error ((t (:foreground "White" :background "Red"))))
 '(flycheck-fringe-warning ((t (:foreground "White" :background "Blue"))))

 '(jedi:highlight-function-argument ((t (:foreground "PaleGreen"))))
 )

(provide-theme 'ack)
;;; ack-theme.el ends here
