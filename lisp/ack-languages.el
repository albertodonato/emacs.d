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
  :hook (go-mode . go-guru-hl-identifier-mode)
  :config
  (setq go-guru-command "~/go/bin/guru"))

(use-package go-projectile)

(use-package groovy-mode)

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package json-mode)
(use-package jsonnet-mode)

(use-package less-css-mode)

(use-package lsp-mode
  :commands lsp
  :bind (:map lsp-mode-map
              ("M-g f" . lsp-format-buffer))
  :hook (python-mode . lsp)
  :config
  (setq lsp-session-file (ack/in-cache-dir "lsp-session")
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil))

(use-package lsp-pyls
  :ensure nil
  :config
  (setq lsp-pyls-plugins-pylint-enabled nil))

(use-package lsp-ui
  :ensure nil
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-position 'at-point))

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

(use-package python-environment
  :ensure nil
  :config
  (setq python-environment-directory "~/virtualenv"
        python-environment-default-root-name "emacs"))

(use-package scss-mode)

(use-package sgml-mode
  :ensure nil
  :config
  ;; reindent after tag is removed
  (advice-add 'sgml-delete-tag :after '(lambda (arg) (indent-region (point-min) (point-max)))))

(use-package web-mode)

(use-package wsd-mode)

(use-package yaml-mode
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent)))

(provide 'ack-languages)
;;; ack-languages.el ends here
