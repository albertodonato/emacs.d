;; ack-languages.el --- Modes for languages

;;; Commentary:
;;;   Load and configure modes for languages.

;;; Code:

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package go-mode
  :bind (:map go-mode-map
              ("M-g f" . gofmt)))

(use-package go-guru
  :hook (go-mode . go-guru-hl-identifier-mode)
  :config
  (setq go-guru-command "~/go/bin/guru"))


(use-package go-projectile)

(use-package jinja2-mode
  :mode "\\.j2\\'")

(use-package lsp
  :hook python-mode)

(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("M-g f" . lsp-format-buffer))
  :config
  (setq lsp-auto-guess-root t))

(use-package lsp-pyls)

(use-package lsp-ui
  :config
  (add-hook 'lsp-ui-mode-hook
            (progn
              (setq lsp-ui-doc-position 'at-point
                    lsp-pyls-plugins-pylint-enabled nil)
              )))

(use-package python
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
  :config
  (setq python-environment-directory "~/virtualenv"
        python-environment-default-root-name "emacs"))

(use-package yaml-mode
  :bind (:map yaml-mode-map
              ("C-m" . newline-and-indent)))

(use-package sgml-mode
  :config
  ;; reindent after tag is removed
  (advice-add 'sgml-delete-tag :after '(lambda (arg) (indent-region (point-min) (point-max)))))

(provide 'ack-languages)
;;; ack-languages.el ends here
