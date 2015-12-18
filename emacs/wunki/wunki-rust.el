(require 'racer)

(setq rust-path "/Users/wunki/Rust"
      racer-rust-src-path (concat rust-path "/rust/src/"))

(defun wunki-rust-mode-hook ()
  (setenv "RUST_SRC_PATH" racer-rust-src-path)
  (set (make-local-variable 'company-backends) '(company-racer))
  (setq tab-width 4))

(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'flycheck-mode)
;(add-hook 'rust-mode-hook #'rustfmt-enable-on-save)

(provide 'wunki-rust)
