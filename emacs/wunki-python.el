;; elpy mode
(when (require 'elpy nil t)
  (elpy-enable))

;; only set this minor modes when using elpy
(setq elpy-default-minor-modes
      '(eldoc-mode flymake-mode auto-complete-mode))

(provide 'wunki-python)
