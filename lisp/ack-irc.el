;;; ack-irc.el --- IRC configuration.

;;; Commentary:
;;;   ERC configuration and additional IRC-related commands.

;;; Code:

(use-package erc
  :ensure nil
  :custom
  (erc-query-display 'buffer)
  (erc-prompt (lambda () (concat (or (erc-default-target) "ERC") ">")))
  (erc-prompt-for-password nil)
  (erc-join-buffer 'bury)
  (erc-rename-buffers nil)
  (erc-server-reconnect-attempts 5)
  (erc-user-full-name ack/erc-user-full-name)
  (erc-email-userid ack/erc-email-userid)
  ;; list all modules so that the call `erc-update-modules' enables all minor
  ;; modes
  (erc-modules '(autojoin
                 button completion dcc fill hl-nicks irccontrols list log
                 match menu move-to-prompt netsplit networks noncommands
                 notifications pcomplete readonly ring scrolltobottom
                 stamp track truncate))
  :config
  (erc-update-modules))

(use-package erc-fill
  :ensure nil
  :custom
  (erc-fill-column 92))

(use-package erc-hl-nicks)

(use-package erc-desktop-notifications
  :ensure nil
  :custom
  (erc-notifications-icon "/snap/emacs/current/usr/share/icons/hicolor/scalable/apps/emacs.svg"))

(use-package erc-button
  :ensure nil
  :custom
  ;; don't include parenthesis in the url
  (erc-button-url-regexp (concat "\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)"
                                 "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
                                 "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]")))

(use-package erc-stamp
  :ensure nil
  :custom
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-stamp-mode t)
  (erc-timestamp-format "[%H:%M] ")
  (erc-timestamp-only-if-changed-flag nil))

(use-package erc-log
  :ensure nil
  :custom
  (erc-log-channels-directory "~/.erc-log")
  (erc-log-matches-types-alist '((keyword . "ERC Notifications") (current-nick . "ERC Notifications")))
  (erc-log-matches-flag t)
  (erc-save-buffer-on-part t)
  (erc-save-queries-on-quit t)
  (erc-log-write-after-send t)
  (erc-log-write-after-insert t))

(use-package erc-track
  :ensure nil
  :custom
  (erc-track-position-in-mode-line nil)
  (erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                             "324" "329" "332" "333" "353" "477"))
  (erc-track-exclude-server-buffer t)
  (erc-track-faces-priority-list '(erc-current-nick-face erc-keyword-face))
  (erc-track-priority-faces-only 'all))

(use-package erc-truncate
  :ensure nil
  :config (erc-truncate-mode t))

(use-package erc-goodies
  :ensure nil
  :config (erc-scrolltobottom-mode t))

(use-package erc-match
  :ensure nil
  :custom
  (erc-current-nick-highlight-type 'all)
  (erc-keywords ack/erc-keywords)
  (erc-keyword-highlight-type 'all)
  (erc-pal-highlight-type 'all))

(use-package erc-pcomplete
  :ensure nil
  :custom
  (erc-pcomplete-nick-postfix ","))

(use-package znc
  :after (erc)
  :commands (znc-erc znc-all)
  :init
  (let* ((password (secrets-get-secret "Login" "ZNC-ack"))
         (define-net (lambda (net)
                       "Define a network for the NET symbol, with ZNC 'username/network' user."
                       (list net (concat ack/znc-username "/" (symbol-name net)) password))))
    (setq znc-servers
          (list (list ack/znc-server-host ack/znc-server-port t (mapcar define-net ack/znc-networks))))))

(use-package emojify
  :after (erc)
  :hook (erc-mode . emojify-mode)
  :config
  (setq emojify-emojis-dir (ack/in-cache-dir "emojis")))

(defmacro ack/def-erc-message-cmd (command description prefix)
  "Define an ERC COMMAND with a DESCRIPTION and PREFIX text."
  (let ((funcname (intern (concat "erc-cmd-" command))))
    `(defun ,funcname (&rest line)
       ,description
       (erc-send-message (concat ,prefix (string-join line " "))))))

(ack/def-erc-message-cmd "TABLEFLIP" "Flip table and send LINE." "(╯°□°）╯︵ ┻━┻ ")
(ack/def-erc-message-cmd "TABLERESET" "Reset table and send LINE." "┳━┳ ノ( ゜-゜ノ)")

(provide 'ack-irc)
;;; ack-irc.el ends here
