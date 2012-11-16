; Lisp settings
(defun enable-paredit-mode ()
  (paredit-mode +1))

;;colors for parenthesis
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;;paredit everywhere
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(add-hook 'nrepl-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;;nrepl
(add-hook 'slime-repl-mode-hook
          (defun clojure-mode-slime-font-lock ()
            (require 'clojure-mode)
            (let (font-lock-mode)
              (clojure-mode-font-lock-setup))))
(setq nrepl-port 4001)

;;eldoc
;(add-hook 'clojure-mode-hook 'eldoc-mode)
;(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;;nrepl
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(setq nrepl-lein-command "/home/wunki/bin/lein2")
