;;; ack-irc.el --- IRC configuration.

;;; Commentary:
;;;   ERC configuration and additional IRC-related commands.

;;; Code:

(use-package erc
  :config
  (setq erc-modules '(autojoin
                      button completion dcc fill
                      irccontrols list log match menu
                      move-to-prompt netsplit networks
                      noncommands pcomplete readonly ring
                      scrolltobottom services stamp track
                      notifications hl-nicks)
        erc-query-display 'buffer
        erc-prompt (lambda ()
                     (if (and (boundp 'erc-default-recipients) (erc-default-target))
                         (erc-propertize
                          (concat (erc-default-target) ">")
                          'read-only t 'rear-nonsticky t 'front-nonsticky t)
                       (erc-propertize
                        "ERC>" 'read-only t 'rear-nonsticky t 'front-nonsticky t)))
        erc-prompt-for-nickserv-password nil
        erc-prompt-for-password nil
        erc-autojoin-mode t
        erc-networks-mode t
        erc-notifications-mode t
        erc-scrolltobottom-mode t
        erc-services-mode 1
        erc-stamp-mode t
        erc-nickserv-identify-mode 'autodetect
        erc-current-nick-highlight-type 'all
        erc-fill-column 92
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-join-buffer 'bury
        erc-server-reconnect-attempts 5
        erc-keyword-highlight-type 'all
        erc-track-position-in-mode-line nil
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-track-exclude-server-buffer t
        erc-track-faces-priority-list '(erc-current-nick-face erc-keyword-face)
        erc-track-priority-faces-only 'all
        erc-log-channels-directory "~/.erc-log"
        erc-save-buffer-on-part nil
        erc-save-queries-on-quit nil
        erc-log-write-after-send t
        erc-log-write-after-insert t
        erc-notifications-icon "/usr/share/icons/hicolor/scalable/apps/emacs-snapshot.svg"
        ;; don't include parenthesis in the url
        erc-button-url-regexp (concat
                               "\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)"
                               "\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?"
                               "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,]+[-a-zA-Z0-9_=#$@~`%&*+\\/]")
        erc-pal-highlight-type 'all
        erc-pcomplete-nick-postfix ","
        erc-rename-buffers nil
        erc-timestamp-format "[%H:%M] "
        erc-timestamp-only-if-changed-flag nil
        erc-log-matches-types-alist '((keyword . "ERC Notifications") (current-nick . "ERC Notifications"))
        erc-log-matches-flag t
        ;; user configuration
        erc-user-full-name "ack"
        erc-email-userid "ack"
        erc-keywords '("alberto.donato" "albertodonato" "maas-crew"))
    (erc-update-modules))

(use-package erc-view-log
  :config
  (use-package autorevert)
  (use-package erc-log)
  (add-hook 'erc-view-log-mode-hook #'turn-on-auto-revert-tail-mode)
  (add-to-list 'auto-mode-alist
               (cons (format "%s/\.*\\.txt$" (regexp-quote
                                              (expand-file-name erc-log-channels-directory)))
                     'erc-view-log-mode)))

(use-package znc
  :commands (znc-erc znc-all)
  :init
  (use-package secrets)
  (let* ((username "ack")
         (password (secrets-get-secret "Login" "ZNC-ack"))
         (networks '(Canonical Freenode Azzurra))
         (define-net (lambda (net) (list net (concat username "/" (symbol-name net)) password))))
    (setq znc-servers
          (list (list ack-znc-server-host ack-znc-server-port t (mapcar define-net networks))))))

(use-package emojify
  :config
  (setq emojify-emojis-dir (ack/in-cache-dir "emojis"))
  :hook (erc-mode . emojify-mode))

(use-package subr-x)

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
