(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode
          (lambda ()
            (setq tab-width 4)
            (flycheck-mode)))

;; auto-completion and definitions with racer
;; (add-to-list 'load-path "~/rust/racer/editors")
;; (setq racer-rust-src-path "~/rust/src/")
;; (setq racer-cmd "~/rust/racer/target/racer")

;; (eval-after-load "rust-mode" '(require 'racer))

(provide 'wunki-rust)
