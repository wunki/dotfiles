(defun wunki-csharp-mode-hook ()
  (electric-pair-mode 1)
  (subword-mode 1)
  (omnisharp-mode 1)
  (flycheck-mode))

(setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")
(add-hook 'csharp-mode-hook 'wunki-csharp-mode-hook)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(provide 'wunki-csharp)
