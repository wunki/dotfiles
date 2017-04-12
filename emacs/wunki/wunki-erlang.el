(add-hook 'erlang-mode-hook (lambda ()
                              (flycheck-mode t)
                              (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back)
                              (setq erlang-compile-function 'projectile-compile-project)))

(provide 'wunki-erlang)
