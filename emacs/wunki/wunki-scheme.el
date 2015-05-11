;; scheme implementation

;; hooks
(defun wunki-scheme-mode-hook ()
  (define-key scheme-mode-map (kbd "M-q") 'sp-indent-defun))

(add-hook 'scheme-mode-hook 'wunki-scheme-mode-hook)
(add-hook 'racket-mode-hook 'wunki-scheme-mode-hook)

(provide 'wunki-scheme)
