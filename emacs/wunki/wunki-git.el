;; magit settings
(require 'magit)

(setq magit-push-always-verify nil) ; don't ask me to confirm the push

(global-set-key (kbd "C-c g") 'magit-status)

(require 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(provide 'wunki-git)
