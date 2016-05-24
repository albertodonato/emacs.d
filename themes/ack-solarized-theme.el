;;; ack-solarized-theme-el --- Additional customization over the solarized theme.

;;; Commentary:
;;;   My changes over the solarized dark theme.

;;; Code:

(deftheme ack-solarized "Customizations for solarized theme.")

(custom-theme-set-faces
 'ack-solarized
 '(default ((t (:inherit nil :family "Ubuntu Mono" :height 102
                         :background "Black" :foreground "White"))))
 '(fringe ((t (:background "Gray10"))))
 '(hl-line ((t (:background "Gray20"))))
 )

(provide-theme 'ack-solarized)
;;; ack-solarized-theme.el ends here
