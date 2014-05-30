;; scheme implementation

;; default to racket language
(setq geiser-active-implementations '(racket))

;; auto-complete for geiser
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

;; hooks
(defun wunki-scheme-mode-hook ()
  ;(turn-on-pretty-mode)
  (define-key scheme-mode-map (kbd "M-q") 'sp-indent-defun))

(add-hook 'scheme-mode-hook 'wunki-scheme-mode-hook)
(add-hook 'racket-mode-hook 'wunki-scheme-mode-hook)

(provide 'wunki-scheme)
