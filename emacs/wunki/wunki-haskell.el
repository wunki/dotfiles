;; load ghci-ng, which is a lot faster
(when (executable-find "ghci-ng")
  (setq-default haskell-process-args-cabal-repl
                '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng")))

;; cleanup haskell on save
(setq-default haskell-stylish-on-save t)

;; haskell tags
(setq-default haskell-tags-on-save t)


(eval-after-load 'haskell-mode
  '(progn
     (defun wunki-haskell-mode-defaults ()
       (subword-mode +1)
       (haskell-doc-mode)
       (haskell-indentation-mode)
       (interactive-haskell-mode +1))

     (setq wunki-haskell-mode-hook 'wunki-haskell-mode-defaults)
     (add-hook 'haskell-mode-hook (lambda ()
                                    (run-hooks 'wunki-haskell-mode-hook)))))

(provide 'wunki-haskell)
