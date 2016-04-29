;; magit settings
(require 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil) ; don't ask me to confirm the push
(setq magit-commit-show-diff nil) ; don't show the diff when committing

(global-set-key (kbd "C-c g") 'magit-status)

(require 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(if (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files/Git/bin"))

(provide 'wunki-git)
