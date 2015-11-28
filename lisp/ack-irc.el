;;; ack-irc.el --- IRC configuration.

;;; Commentary:
;;;   ERC configuration and additional IRC-related commands.

;;; Code:

(require 'secrets)
(require 'erc)
(require 'erc-services)
(require 'tls)
(require 'erc-log)
(require 'erc-view-log)
(require 'erc-desktop-notifications)
(require 'erc-hl-nicks)
(require 'znc)

(require 'ack-private)

(setq erc-modules
      '(autojoin button completion dcc fill
                 irccontrols list log match menu
                 move-to-prompt netsplit networks
                 noncommands readonly ring
                 scrolltobottom services stamp track
                 notifications hl-nicks))
(setq erc-autojoin-mode t
      erc-networks-mode t
      erc-notifications-mode t
      erc-scrolltobottom-mode t
      erc-services-mode 1
      erc-stamp-mode t)

(setq erc-nickserv-identify-mode 'autodetect)
(setq erc-current-nick-highlight-type 'all)
(setq erc-fill-column 92)
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
(setq erc-join-buffer 'bury)
(setq erc-server-reconnect-attempts 5)
(setq erc-keyword-highlight-type 'all)

(setq erc-log-channels-directory "~/.erc-log")
(setq erc-save-buffer-on-part nil
      erc-save-queries-on-quit nil
      erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-notifications-icon "/usr/share/icons/hicolor/scalable/apps/emacs-snapshot.svg")

(setq erc-pal-highlight-type 'all)
(setq erc-pcomplete-nick-postfix ",")
(setq erc-prompt
      (lambda nil
        (if
            (and
             (boundp
              (quote erc-default-recipients))
             (erc-default-target))
            (erc-propertize
             (concat
              (erc-default-target)
              ">")
             (quote read-only) t
             (quote rear-nonsticky) t
             (quote front-nonsticky) t)
          (erc-propertize
           (concat "ERC>")
           (quote read-only) t
           (quote rear-nonsticky) t
           (quote front-nonsticky) t))))
(setq erc-prompt-for-nickserv-password nil)
(setq erc-prompt-for-password nil)
(setq erc-query-display 'buffer)
(setq erc-rename-buffers nil)
(setq erc-timestamp-format "[%H:%M] ")
(setq erc-timestamp-only-if-changed-flag nil)

;; network and user configuration
(setq erc-user-full-name "ack"
      erc-email-userid "ack"
      erc-keywords '("landscape-crew" "alberto"))

(add-hook 'erc-view-log-mode-hook 'turn-on-auto-revert-tail-mode)

(let* ((username "ack")
       (password (secrets-get-secret "Login" "ZNC-ack"))
       (networks '(Canonical Freenode Azzurra))
       (define-net (lambda (net)
                     (list net (concat username "/" (symbol-name net)) password))))
  (setq znc-servers
        (list (list ack-znc-server-host ack-znc-server-port t (mapcar define-net networks)))))

(provide 'ack-irc)

;;; ack-irc.el ends here
