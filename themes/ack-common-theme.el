;;; ack-common-theme-el --- Customization shared by multiple themes.

;;; Commentary:
;;;   Customizations that apply to multiple themes.

;;; Code:

(deftheme ack-common "Customizations shared by multiple themes.")

(custom-theme-set-faces
 'ack-common
 '(erc-current-nick-face ((t (:foreground "Yellow"))))
 '(erc-input-face ((t (:foreground "DimGray"))))
 '(erc-my-nick-face ((t (:foreground "LightGray"))))
 '(erc-prompt-face ((t (:inherit 'erc-default-face :foreground "DarkGray"))))
 '(erc-timestamp-face ((t (:foreground "DimGray"))))
 )

(provide-theme 'ack-common)
;;; ack-common-theme.el ends here
