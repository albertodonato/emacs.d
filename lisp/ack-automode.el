;;; ack-automode.el -- Automode file types

;;; Commentary:
;;;   Automode binding for file extensions.

;;; Code:

(require 'python)
(require 'yaml-mode)
(require 'js)
(require 'erc-log)
(require 'erc-view-log)

;; YAML files
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; React JSX files
(add-to-list 'auto-mode-alist '("\\.jsx$" . js-jsx-mode))
;; Twisted tac files
(add-to-list 'auto-mode-alist '("\\.tac$" . python-mode))
;; Zope files
(add-to-list 'auto-mode-alist '("\\.zcml$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.pt$" . html-mode))
;; Jinja2 files
(add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))
;; ERC log files
(add-to-list 'auto-mode-alist
             (cons (format "%s/\.*\\.txt$" (regexp-quote
                                            (expand-file-name erc-log-channels-directory)))
                   'erc-view-log-mode))

(provide 'ack-automode)
;;; ack-automode.el ends here

