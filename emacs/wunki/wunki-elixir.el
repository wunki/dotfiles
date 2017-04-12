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
  (flycheck-mix-setup)
  (flycheck-mode))

;; don't use C-c a for alchemist keys since it clashes with org-mode
(setq alchemist-key-command-prefix (kbd "C-c ,"))

;; source code of erlang and elixir
(setq alchemist-goto-erlang-source-dir "~/projects/otp/")
(setq alchemist-goto-elixir-source-dir "~/projects/elixir/")

(add-hook 'elixir-mode-hook 'wunki-elixir-mode-hook)
(provide 'wunki-elixir)
