
(require 'lsp-mode)
(require 'lsp-flycheck)

(with-eval-after-load 'lsp-mode
  (setq lsp-rust-rls-command '("rustup" "run" "nightly-2017-12-01" "rls"))
  (require 'lsp-rust))

(defun wunki-rust-mode-hook ()
  (setq lsp-highlight-symbol-at-point nil)
  (setq rust-format-on-save t)
  (lsp-rust-enable)
  (flycheck-mode))

(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)

(provide 'wunki-rust)
