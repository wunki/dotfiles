(require 'lsp-rust)
(require 'lsp-mode)
(require 'lsp-flycheck)
(defun wunki-rust-mode-hook ()
  (lsp-mode)
  (flycheck-mode))

(setq lsp-highlight-symbol-at-point nil)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)

(provide 'wunki-rust)
