;;; ack-landscape.el --- Landscape testing configuration

;;; Commentary:
;;;   Landscape testing configuration.

;;; Code:

(require 'landscape)

(setq ls-code-root (concat (file-name-as-directory (getenv "HOME")) "canonical/src")
      ls-shell-source-file "~/system/source/landscape"
      ls-lxc-container-name "landscape-xenial")

(provide 'ack-landscape)
;;; ack-landscape.el ends here
