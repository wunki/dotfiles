(defun wunki-rust-mode-hook ()
  (require 'lsp-rust)
  (require 'lsp-mode)
  (setq lsp-highlight-symbol-at-point nil)
  (lsp-mode)
  (require 'lsp-flycheck)
  (flycheck-mode))

(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)

(provide 'wunki-rust)
