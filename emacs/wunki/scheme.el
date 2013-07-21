;; settings for scheme implentations, Racket language.
(require 'pretty-mode-plus)
(add-hook 'racket-mode-hook 'turn-on-pretty-mode)
;; default to racket language
(setq geiser-active-implementations '(racket))

;; enable paredit on geiser
(add-hook 'geiser-repl-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
