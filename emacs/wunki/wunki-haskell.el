;; Format on save
(setq-default haskell-stylish-on-save t)
 
(eval-after-load 'haskell-mode
  '(progn
     (defun wunki-haskell-mode-defaults ()
       (subword-mode +1)
       (hindent-mode)
       (intero-mode))

     (setq wunki-haskell-mode-hook 'wunki-haskell-mode-defaults)
     (add-hook 'haskell-mode-hook (lambda ()
                                    (run-hooks 'wunki-haskell-mode-hook)))))

;; hlint as warnings
(with-eval-after-load 'intero
    (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

(provide 'wunki-haskell)
