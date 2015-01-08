;; magit settings
(require 'magit)

(global-set-key (kbd "C-c g") 'magit-status)

;; store windows when invoking magit
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

;; restore windows when quiting
(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (when (get-register :magit-fullscreen)
    (jump-to-register :magit-fullscreen)))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'wunki-git)

