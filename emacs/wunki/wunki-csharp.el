(defun wunki-csharp-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)
  (electric-pair-mode 1)
  (subword-mode 1)
  (omnisharp-mode 1)
  (flycheck-mode))

(add-hook 'csharp-mode-hook 'wunki-csharp-mode-hook)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(provide 'wunki-csharp)
