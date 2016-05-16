(require 'racer)

(defun wunki-rust-mode-hook ()
  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "~/.etc/rust/src/")
  (rustfmt-enable-on-save)
  (set (make-local-variable 'company-backends) '(company-racer))
  (setq tab-width 4))

(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'flycheck-mode)

(provide 'wunki-rust)
