;; modes which improve the experience
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook 'flycheck-mode)
(add-hook 'csharp-mode-hook 'yas-minor-mode)
(add-hook 'csharp-mode-hook 'eldoc-mode)

(setq omnisharp-server-executable-path "/Users/wunki/.etc/omnisharp-roslyn/artifacts/publish/OmniSharp/osx.10.11-x64/dnx451/OmniSharp.exe")

;; autocompletion
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(provide 'wunki-csharp)
