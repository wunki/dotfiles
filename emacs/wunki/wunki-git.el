;; magit settings
(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil) ; don't ask me to confirm the push
(setq magit-revert-buffers 'silent) ; just change, don't tell

(global-set-key (kbd "C-c g") 'magit-status)

(require 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(provide 'wunki-git)
