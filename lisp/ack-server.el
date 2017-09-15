;;; ack-server.el --- Emacs server setup

;;; Commentary:
;;;   Helper functions and variables for Emacs initialization.

;;; Code:

(require 'server)

(when (and (functionp 'server-running-p) (not (server-running-p)))
  (server-start))

(provide 'ack-server)
;;; ack-server.el ends here
