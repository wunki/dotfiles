(add-hook 'rust-mode
          (lambda ()
            (setq tab-width 4)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; auto-completion and definitions with racer
(setq racer-rust-src-path "/Users/wunki/src/rust/src/")
(setq racer-cmd "/Users/wunki/Source/racer/target/racer")
(if (mac?)
    (add-to-list 'load-path "/Users/wunki/Source/racer/editors")
  (add-to-list 'load-path "/home/wunki/src/racer/editors"))
(eval-after-load "rust-mode" '(require 'racer))

(provide 'wunki-rust)
