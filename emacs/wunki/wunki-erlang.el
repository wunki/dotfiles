(add-hook 'erlang-mode-hook (lambda ()
                              (flycheck-mode t)
                              (setq erlang-compile-function 'projectile-compile-project)))

(provide 'wunki-erlang)
