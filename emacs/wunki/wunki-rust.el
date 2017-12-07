(require 'lsp-mode)
(require 'lsp-flycheck)

(with-eval-after-load 'lsp-mode
  (require 'lsp-rust))

(defun wunki-rust-mode-hook ()
  (setq lsp-highlight-symbol-at-point t)
  (setq rust-format-on-save t)
  (lsp-rust-enable)
  (flycheck-mode))

(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)

(provide 'wunki-rust)
