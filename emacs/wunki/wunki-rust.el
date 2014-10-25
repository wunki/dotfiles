(add-hook 'rust-mode
          (lambda ()
            (setq tab-width 4)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'wunki-rust)
