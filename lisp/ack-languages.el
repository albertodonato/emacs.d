;; ack-languages.el --- Modes for languages

;;; Commentary:
;;;   Load and configure modes for languages.

;;; Code:

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package crontab-mode)

(use-package dpkg-dev-el)

(use-package dockerfile-mode)

(use-package go-mode
  :bind (:map go-mode-map
              ("M-g f" . gofmt)))

(use-package go-guru
  :after (go-mode)
  :hook (go-mode . go-guru-hl-identifier-mode)
  :config
  (setq go-guru-command "~/go/bin/guru"))

(use-package go-projectile)

(use-package groovy-mode)

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package json-mode)

(use-package jsonnet-mode)

(use-package ledger-mode
  :config
  (setq ledger-post-amount-alignment-column 65))

(use-package less-css-mode)

(use-package lsp-mode
  :commands (lsp)
  :bind (:map lsp-mode-map
              ("M-g f" . lsp-format-buffer))
  :hook ((python-mode . (lambda () (require 'lsp-python-ms) (lsp))) (go-mode . lsp))
  :config
  (setq lsp-session-file (ack/in-cache-dir "lsp-session")
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-headerline-breadcrumb-segments '(symbols)
        lsp-headerline-breadcrumb-mode t))

(use-package lsp-ui
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-position 'at-point))

(use-package lsp-pylsp
  :ensure nil
  :config
  (setq lsp-pylsp-server-command "/snap/bin/pylsp"
        lsp-pylsp-plugins-flake8-max-line-length fill-column
        lsp-pylsp-plugins-pydocstyle-enabled nil
        lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t))))

(use-package lua-mode)

(use-package markdown-mode)

(use-package nginx-mode)

(use-package po-mode)

(use-package python
  :ensure nil
  :mode (("\\.tac\\'" . python-mode))
  :bind (:map python-mode-map
              ("C-c d" . (lambda ()
                          "Insert a pdb statement."
                          (interactive)
                          (ack/insert-text-and-reindent "import pdb; pdb.set_trace()"))))
  :config
  (setq python-shell-interpreter "python3"
        python-indent-offset 4
        python-indent-guess-indent-offset-verbose nil))

(use-package scss-mode)

(use-package sgml-mode
  :ensure nil
  :config
  ;; reindent after tag is removed
  (advice-add 'sgml-delete-tag :after #'(lambda (arg) (indent-region (point-min) (point-max)))))

(use-package web-mode)

(use-package wsd-mode)

(use-package yaml-mode
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent)))

(provide 'ack-languages)
;;; ack-languages.el ends here
