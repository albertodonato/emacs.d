;;; ack-irc.el --- IRC configuration.

;;; Commentary:
;;;   ERC configuration and additional IRC-related commands.

;;; Code:

(require 'jabber)

(setq jabber-auto-reconnect t
      jabber-avatar-verbose nil
      jabber-mode-line-mode t
      jabber-vcard-avatars-retrieve nil
      ;;jabber-chat-buffer-format "*-jabber-%n-*"
      jabber-roster-buffer "*-jabber-*"
      jabber-roster-line-format " %c %-25n %u %-8s (%r)"
      jabber-show-offline-contacts nil)

(setq jabber-account-list
      `(("alberto.donato@gmail.com")
        (:network-server . "talk.google.com")
        (:password . ,(secrets-get-secret "Login" "jabber-ack"))))

(provide 'ack-jabber)
;;; ack-jabber.el ends here
