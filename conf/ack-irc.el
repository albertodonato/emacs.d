;;; ack-irc.el --- IRC configuration.

;; Copyright (C) 2015  Alberto Donato

;; Author: Alberto Donato <alberto.donato@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ERC configuration and additional IRC-related commands.

;;; Code:

;; ERC configuration
(require 'erc)
(require 'erc-services)
(require 'tls)
(require 'erc-view-log)
(require 'erc-desktop-notifications)
(require 'erc-highlight-nicknames)

;; ERC settings for networks
(setq erc-autojoin-mode t)
(setq erc-current-nick-highlight-type 'all)
(setq erc-fill-column 100)
(setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
(setq erc-join-buffer 'bury)
(setq erc-keyword-highlight-type 'all)
(setq erc-log-channels-directory "~/.erc-log")
(setq erc-modules
      '(autojoin button completion dcc fill
		 irccontrols list log match menu
		 move-to-prompt netsplit networks
		 noncommands readonly ring
		 scrolltobottom services stamp track
		 notifications highlight-nicknames))
(setq erc-networks-mode t)
(setq erc-nickserv-identify-mode 'autodetect)
(setq erc-notifications-icon "/usr/share/icons/hicolor/scalable/apps/emacs-snapshot.svg")
(setq erc-notifications-mode t)
(setq erc-notify-mode t)
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
(setq erc-scrolltobottom-mode t)
(setq erc-services-mode 1)
(setq erc-speedbar-sort-users-type 'alphabetical)
(setq erc-stamp-mode t)
(setq erc-timestamp-format "[%H:%M] ")
(setq erc-timestamp-only-if-changed-flag nil)
;; network and user configuration
(setq erc-user-full-name "ack")
(setq erc-email-userid "ack")
(setq erc-networks-alist
      '((Azzurra "azzurra.net")
	(Canonical "canonical.com")
	(freenode "freenode.net")))
(setq erc-server-alist
      '(("Azzurra" Azzurra "irc.azzurra.net" 6667)
	("Canonical" Canonical "irc.canonical.com" 6667)
	("Freenode: Random server" freenode "irc.freenode.net" 6667)
	("Freenode: Random EU server" freenode "irc.eu.freenode.net" 6667)
	("Freenode: Random US server" freenode "irc.us.freenode.net" 6667)))
(setq erc-autojoin-channels-alist
      '(("canonical.com" "#Cisco" "#landscape-pvt" "#eco" "#landscape"
	 "#cloud-dev" "#juju" "#hr" "#webops" "#canonical" "#italia" "#is"
	 "#is-outage" "#maas" "#server")
	("freenode.net" "#la-it")
	("azzurra.org" "#retrocomputing")))
(setq erc-nickserv-alist
      '((Azzurra "NickServ!service@azzurra.org" "/ns\\s-IDENTIFY\\s-password"
		 "NickServ" "IDENTIFY" nil nil nil)
	(freenode "NickServ!NickServ@services."
		  "/msg\\s-NickServ\\s-identify\\s-<password>" "NickServ"
		  "IDENTIFY" nil nil "You\\s-are\\s-now\\s-idenfified")))
(setq erc-keywords '("landscape-crew" "alberto"))

;; Settings for erc-view-log
(add-to-list 'auto-mode-alist `(,(format "%s/.*\\.log" (regexp-quote (expand-file-name erc-log-channels-directory))) . erc-view-log-mode))
(add-hook 'erc-view-log-mode-hook 'turn-on-auto-revert-tail-mode)

;; Helper functions to connect to IRC
(defun irc-network-password (network)
  "Return the NETWORK password for an IRC network."
  (cdr (assoc network irc-network-passwords)))

(defun irc-bip (network port)
  "Connect to a NETWORK on the specified PORT via Bip proxy."
  (erc
   :server "localhost"
   :port port
   :nick "ack"
   :full-name "ack"
   :password (format "ack:%s:%s" (irc-network-password 'Ack) network)))

(defun irc-freenode ()
  "Connect to Freenode IRC network."
  (interactive)
  (erc-tls
   :server "irc.freenode.net"
   :port 6697
   :nick "ackk"
   :full-name "ack"))

(defun irc-azzurra ()
  "Connect to Azzurra IRC network."
  (interactive)
  (erc
   :server "irc.eu.azzurra.org"
   :port 6667
   :nick "ack"
   :full-name "ack"))

(defun irc-canonical ()
  "Connect to Canonical IRC network."
  (interactive)
  (erc-tls
   :server "irc.canonical.com"
   :port 6697
   :nick "ack"
   :full-name "Alberto Donato"
   :password (irc-network-password 'Canonical)))

(defun irc-connect ()
  "Connect to all IRC networks."
  (interactive)
  (irc-bip "canonical" 7778)
  (irc-bip "freenode" 7779)
  (irc-bip "azzurra" 7780))

;; rcirc configuration
(setq rcirc-prompt "%n@%t> ")

(provide 'ack-irc)

;;; ack-irc.el ends here
