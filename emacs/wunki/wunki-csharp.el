(defun wunki-csharp-mode-hook ()
  (electric-pair-mode 1)
  (subword-mode 1))
(add-hook 'csharp-mode-hook 'wunki-csharp-mode-hook)

(provide 'wunki-csharp)
