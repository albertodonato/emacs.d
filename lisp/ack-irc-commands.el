;;; ack-irc-commands.el --- Custom IRC commands.

;;; Commentary:
;;;   Custom commands for ERC.

;;; Code:

(require 'erc)
(require 'subr-x)

(defun erc-cmd-TABLEFLIP (&rest line)
  "Flip table and send LINE."
  (erc-send-message (concat "(╯°□°）╯︵ ┻━┻ " (string-join line " "))))

(provide 'ack-irc-commands)
;;; ack-irc-commands.el ends here
