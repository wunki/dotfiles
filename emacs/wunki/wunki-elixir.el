(require 'elixir-mode)

(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
         :when '(("SPC" "RET"))
         :actions '(insert navigate))
  (sp-local-pair "do" "end"
         :when '(("SPC" "RET"))
         :post-handlers '(sp-ruby-def-post-handler)
         :actions '(insert navigate)))

(defun wunki-elixir-mode-hook ()
  (alchemist-mode)
  (flycheck-mix-setup))

(add-hook 'elixir-mode-hook 'wunki-elixir-mode-hook)

(provide 'wunki-elixir)
