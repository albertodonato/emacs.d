;;; ack-ample-theme-el --- Additional customization over the ample theme.

;;; Commentary:
;;;   My changes on top of the ample theme.

;;; Code:

(deftheme ack-ample "Customizations for ample theme.")

(custom-theme-set-faces
 'ack-ample

 '(hl-line ((t (:background "Gray20"))))
 '(region ((t (:background "Gray40"))))
 )

(provide-theme 'ack-ample)
;;; ack-ample-theme.el ends here
