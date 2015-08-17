;; magit settings
(require 'magit)

(global-set-key (kbd "C-c g") 'magit-status)

;(require 'fullframe)
;(after-load 'magit
;  (fullframe magit-status magit-mode-quit-window))

(provide 'wunki-git)
