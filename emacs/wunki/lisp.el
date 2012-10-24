; Lisp settings

;; hooks
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'clojure-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'turn-on-paredit)
(add-hook 'emacs-lisp-mode-hook 'turn-on-paredit)
(add-hook 'repl-mode-hook 'turn-on-paredit)

;; nrepl
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
;(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-mode-hook 'turn-on-paredit)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(setq nrepl-lein-command "lein")
