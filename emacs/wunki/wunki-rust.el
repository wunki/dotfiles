(defun wunki-rust-mode-hook ()
  (setq racer-rust-src-path "/Users/wunki/Rust/rust/src/"
        racer-cmd "/Users/wunki/Rust/racer/target/release/racer")
  (add-to-list 'load-path "/Users/wunki/Rust/racer/editors/emacs")
  (setq tab-width 4)
  (require 'racer))

(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook 'flycheck-rust-setup)
(add-hook 'rust-mode-hook 'wunki-rust-mode-hook)

(provide 'wunki-rust)
