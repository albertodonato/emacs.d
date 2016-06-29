;;; ack-solarized-theme-el --- Additional customization over the solarized theme.

;;; Commentary:
;;;   My changes on top of the solarized dark theme.

;;; Code:

(deftheme ack-solarized "Customizations for solarized theme.")

(custom-theme-set-faces
 'ack-solarized
 '(default ((t (:inherit nil :family "Ubuntu Mono" :height 102
                         :background "Black" :foreground "Gray80"))))
 '(fringe ((t (:background "Gray10"))))
 '(hl-line ((t (:background "Gray20"))))
 '(error ((t (:foreground "Red1" :inverse-video nil))))

 '(linum ((t (:background "Black"))))

 '(diff-header ((t (:background "Black"))))
 '(diff-file-header ((t (:background "Black" :foreground "#b58900"))))
 '(diff-added ((t (:background "Black"))))
 '(diff-removed ((t (:background "Black"))))

 '(jedi:highlight-function-argument ((t (:foreground "#2aa198"))))
 )

(provide-theme 'ack-solarized)
;;; ack-solarized-theme.el ends here
