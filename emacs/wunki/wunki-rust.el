(require 'racer)

(setq rust-path (if (mac?) "/Users/wunki/Rust" "/home/wunki/rust")
      racer-rust-src-path (concat rust-path "/rust/src/")
      racer-cmd (concat rust-path "/racer/target/release/racer")
      company-racer-executable (concat rust-path "/racer/target/release/racer"))

(defun wunki-rust-mode-hook ()
  (setenv "RUST_SRC_PATH" racer-rust-src-path)
  (set (make-local-variable 'company-backends) '(company-racer))
  (setq tab-width 4))

(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'flycheck-mode)

(provide 'wunki-rust)
