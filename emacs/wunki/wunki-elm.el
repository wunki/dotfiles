(defun wunki-elm-mode-hook ()
  (elm-oracle-setup-completion)
  (set (make-local-variable 'company-backends) '(company-elm)))

(add-hook 'elm-mode-hook 'wunki-elm-mode-hook)

(provide 'wunki-elm)
