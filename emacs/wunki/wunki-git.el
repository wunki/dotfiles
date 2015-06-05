;; magit settings
(require 'magit)

(global-set-key (kbd "C-c g") 'magit-status)

(require 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'wunki-git)

