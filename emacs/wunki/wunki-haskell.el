(defun wunki-haskell-mode-hook ()
  (local-set-key (kbd "C-c C-b") 'haskell-interactive-bring)
  (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build)
  (local-set-key (kbd "C-c C-i") 'haskell-process-do-info)
  (local-set-key (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (local-set-key (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (local-set-key (kbd "C-c C-s") 'haskell-interactive-switch)
  (local-set-key (kbd "C-c C-t") 'haskell-process-do-type)
  (local-set-key (kbd "C-c c")   'haskell-process-cabal)
  (local-set-key (kbd "C-c v c") 'haskell-cabal-visit-file)
  (local-set-key (kbd "SPC")     'haskell-mode-contextual-space)
  ;; haskell-flycheck
  (delete 'haskell-ghc flycheck-checkers)
  (require 'haskell-flycheck))

(add-hook 'haskell-mode-hook 'hindent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'wunki-haskell-mode-hook)

(setq haskell-process-auto-import-loaded-modules t
      haskell-process-log t
      haskell-process-type 'stack-ghci
      haskell-process-suggest-remove-import-lines t)

(provide 'wunki-haskell)
