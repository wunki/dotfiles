;; default to racket language
(setq geiser-active-implementations '(racket))

;; enable paredit on geiser
(add-hook 'geiser-repl-mode-hook 'enable-paredit-mode)
