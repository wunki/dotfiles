;; modes which improve the experience
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'flycheck-mode)
(add-hook 'csharp-mode-hook 'yas-minor-mode)
(add-hook 'csharp-mode-hook 'eldoc-mode)

;; autocompletion
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(provide 'wunki-csharp)
