;;; ack-irc.el --- IRC configuration.

;;; Commentary:
;;;   ERC configuration and additional IRC-related commands.

;;; Code:

(use-package erc
  :ensure nil
  :config
  (use-package erc-desktop-notifications
    :ensure nil
    :config
    (setq erc-notifications-icon "/usr/share/icons/hicolor/scalable/apps/emacs-snapshot.svg"))
  (use-package erc-goodies
    :ensure nil
    :config
    (setq erc-scrolltobottom-mode t))
  (use-package erc-button
    :ensure nil
    :config
    (setq erc-button-url-regexp (concat ;; don't include parenthesis in the url
                                 "\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)"
                                 "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
                                 "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]")))
  (use-package erc-fill
    :ensure nil
    :config
    (setq erc-fill-column 92))
  (use-package erc-hl-nicks)
  (use-package erc-log
    :ensure nil
    :config
    (setq erc-log-channels-directory "~/.erc-log"
          erc-log-matches-types-alist '((keyword . "ERC Notifications") (current-nick . "ERC Notifications"))
          erc-log-matches-flag t
          erc-save-buffer-on-part t
          erc-save-queries-on-quit t
          erc-log-write-after-send t
          erc-log-write-after-insert t))
  (use-package erc-match
    :ensure nil
    :config
    (setq erc-current-nick-highlight-type 'all
          erc-keywords ack/erc-keywords
          erc-keyword-highlight-type 'all
          erc-pal-highlight-type 'all))
  (use-package erc-pcomplete
    :ensure nil
    :config
    (setq erc-pcomplete-nick-postfix ","))
  (use-package erc-services
    :ensure nil
    :config
    (setq erc-prompt-for-nickserv-password nil
          erc-nickserv-identify-mode 'autodetect))
  (use-package erc-stamp
    :ensure nil
    :config
    (setq erc-insert-timestamp-function 'erc-insert-timestamp-left
          erc-stamp-mode t
          erc-timestamp-format "[%H:%M] "
          erc-timestamp-only-if-changed-flag nil))
  (use-package erc-track
    :ensure nil
    :config
    (setq erc-track-position-in-mode-line nil
          erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477")
          erc-track-exclude-server-buffer t
          erc-track-faces-priority-list '(erc-current-nick-face erc-keyword-face)
          erc-track-priority-faces-only 'all))
  (use-package erc-truncate
    :ensure nil
    :config
    (setq erc-truncate-mode t))
  (setq erc-query-display 'buffer
        erc-prompt (lambda ()
                     (if (and (boundp 'erc-default-recipients) (erc-default-target))
                         (erc-propertize
                          (concat (erc-default-target) ">")
                          'read-only t 'rear-nonsticky t 'front-nonsticky t)
                       (erc-propertize
                        "ERC>" 'read-only t 'rear-nonsticky t 'front-nonsticky t)))
        erc-prompt-for-password nil
        erc-join-buffer 'bury
        erc-rename-buffers nil
        erc-server-reconnect-attempts 5
        erc-user-full-name ack/erc-user-full-name
        erc-email-userid ack/erc-email-userid
        ;; list all modules so that the call `erc-update-modules' enables all
        ;; minor modes
        erc-modules '(autojoin
                      button completion dcc fill hl-nicks irccontrols list log
                      match menu move-to-prompt netsplit networks noncommands
                      notifications pcomplete readonly ring scrolltobottom
                      services stamp track truncate))
  (erc-update-modules))

(use-package erc-view-log
  :after (erc)
  :hook ((erc-view-log-mode . turn-on-auto-revert-tail-mode))
  :config
  (use-package autorevert
    :ensure nil)
  (add-to-list 'auto-mode-alist
               (cons (format "%s/\.*\\.txt$" (regexp-quote
                                              (expand-file-name erc-log-channels-directory)))
                     'erc-view-log-mode)))

(use-package znc
  :after (erc)
  :commands (znc-erc znc-all)
  :init
  (use-package secrets
    :ensure nil)
  (let* ((password (secrets-get-secret "Login" "ZNC-ack"))
         (define-net (lambda (net)
                       "Define a network for the NET symbol, with ZNC 'username/network' user."
                       (list net (concat ack/znc-username "/" (symbol-name net)) password))))
    (setq znc-servers
          (list (list ack/znc-server-host ack/znc-server-port t (mapcar define-net ack/znc-networks))))))

(use-package emojify
  :after (erc)
  :config
  (setq emojify-emojis-dir (ack/in-cache-dir "emojis"))
  :hook (erc-mode . emojify-mode))

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
