;; ack-languages.el --- Modes for languages

;;; Commentary:
;;;   Load and configure modes for languages.

;;; Code:

(use-package bash-completion
  :init
  (bash-completion-setup))

(use-package crontab-mode)

(use-package dpkg-dev-el)

(use-package dockerfile-mode)

(use-package go-mode
  :hook (go-mode . (lambda () (require 'lsp-go) (lsp)))
  :bind (:map go-mode-map
              ("M-g f" . gofmt)))

(use-package go-projectile)

(use-package graphql-mode)

(use-package groovy-mode)

(use-package hcl-mode)

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package json-mode)

(use-package jsonnet-mode)

(use-package just-mode)

(use-package ledger-mode
  :custom
  (ledger-post-amount-alignment-column 65))

(use-package less-css-mode)

(use-package lsp-mode
  :commands (lsp)
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind (:map lsp-mode-map
              ("M-g f" . lsp-format-buffer))
  :custom
  (lsp-session-file (ack/in-cache-dir "lsp-session"))
  (lsp-enable-links t)
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  (lsp-file-watch-threshold nil)
  (lsp-log-io nil))

(use-package lsp-headerline
  :ensure nil
  :custom
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-headerline-breadcrumb-mode t))

(use-package lsp-ui
  :commands (lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-doc-position 'at-point))

(use-package lsp-pylsp
  :ensure nil
  :hook (python-mode . lsp)
  :custom
  (lsp-pylsp-plugins-black-enabled t)
  (lsp-pylsp-plugins-mccabe-enabled nil)
  (lsp-pylsp-plugins-pydocstyle-enabled nil)
  (lsp-pylsp-plugins-pyflakes-enabled nil))

(use-package lsp-rust
  :ensure nil
  :hook (rust-mode . lsp))

(use-package lsp-nix
  :ensure nil
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package lsp-terraform-ls
  :ensure nil
  :hook (terraform-mode . lsp)
  :custom
  (lsp-terraform-ls-prefill-required-fields t)
  (lsp-terraform-ls-server (ack/availble-executable '("opentofu-ls" "terraform-ls"))))

(use-package nix-mode
  :ensure t
  :hook (nix-mode . lsp-deferred))

(use-package lua-mode)

(use-package markdown-mode)

(use-package nginx-mode)

(use-package po-mode)

(use-package python
  :ensure nil
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  (python-indent-guess-indent-offset-verbose nil))

(use-package rust-mode)

(use-package scss-mode)

(use-package sgml-mode
  :ensure nil
  :config
  ;; reindent after tag is removed
  (advice-add 'sgml-delete-tag :after #'(lambda (arg) (indent-region (point-min) (point-max)))))

(use-package terraform-mode)

(use-package web-mode)

(use-package wsd-mode)

(use-package yaml-mode
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent)))

(use-package yasnippet
  :init (yas-global-mode t))

(provide 'ack-languages)
;;; ack-languages.el ends here
