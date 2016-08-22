(add-hook 'c-mode-common-hook (lambda ()
                                (google-set-c-style)
                                (google-make-newline-indent)
                                (flycheck-mode t)))

(provide 'wunki-c)
