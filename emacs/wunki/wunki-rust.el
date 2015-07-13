(setq rust-path (if (mac?)
                    "/Users/wunki/Rust"
                  "/home/wunki/rust"))

(defun wunki-rust-mode-hook ()
  (setq racer-rust-src-path (concat rust-path "/rust/src/")
        racer-cmd (concat rust-path "/racer/target/release/racer"))
  (setq tab-width 4))

(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)

(provide 'wunki-rust)
