;; magit settings
(require 'magit)

(setq magit-push-always-verify nil) ; don't ask me to confirm the push
(setq magit-diff-auto-show '(stage-all log-oneline log-select blame-follow)) ; don't show diff on commit

(global-set-key (kbd "C-c g") 'magit-status)

(require 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(provide 'wunki-git)
