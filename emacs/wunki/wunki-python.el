;; elpy mode
(when (require 'elpy nil t)
  (elpy-enable))

;; only set this minor modes when using elpy
(setq elpy-modules
      '(elpy-module-sane-defaults elpy-module-company elpy-module-eldoc elpy-module-flycheck elpy-module-pyvenv))

(provide 'wunki-python)
