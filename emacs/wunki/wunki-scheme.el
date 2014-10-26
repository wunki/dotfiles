;; scheme implementation

;; default to racket language
(setq geiser-active-implementations '(racket))

;; hooks
(defun wunki-scheme-mode-hook ()
  ;(turn-on-pretty-mode)
  (define-key scheme-mode-map (kbd "M-q") 'sp-indent-defun))

(add-hook 'scheme-mode-hook 'wunki-scheme-mode-hook)
(add-hook 'racket-mode-hook 'wunki-scheme-mode-hook)

(provide 'wunki-scheme)
