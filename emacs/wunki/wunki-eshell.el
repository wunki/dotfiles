;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))
(setq eshell-banner-message "")

;; Define a pretty prompt.
(eshell-git-prompt-use-theme 'powerline)

;; Disable line highligthing
(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local global-hl-line-mode nil)
                              (define-key eshell-mode-map (kbd "M-p") 'ace-window)))

(setq eshell-cmpl-cycle-completions nil)

;; quickly fire up e-shell.
(global-set-key (kbd "C-c C-e") 'eshell)

(provide 'wunki-eshell)
