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

 '(linum ((t (:background "Black"))))

 '(diff-header ((t (:background "Black"))))
 '(diff-file-header ((t (:background "Black" :foreground "#b58900"))))
 )

(provide-theme 'ack-solarized)
;;; ack-solarized-theme.el ends here
