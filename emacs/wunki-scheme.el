;; scheme implementation
(setq scheme-program-name "csi -:c")

;; pretty scheme
(require 'pretty-mode-plus)
(add-hook 'racket-mode-hook 'turn-on-pretty-mode)

;; default to racket language
(setq geiser-active-implementations '(racket))

;; auto-complete for geiser
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;; keybindings
(define-key scheme-mode-map (kbd "M-q") 'sp-indent-defun)

(provide 'wunki-scheme)
