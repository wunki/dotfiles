(add-hook 'rust-mode
          (lambda ()
            (setq tab-width 4)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; auto-completion and definitions with racer
(add-to-list 'load-path "/home/wunki/rust/racer/editors")
(require 'racer)

(provide 'wunki-rust)
