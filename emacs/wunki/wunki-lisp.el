;; lisp languages (mainly clojure)

;; enable this on lisp languages
(defun lisp-coding-defaults ()
  (subword-mode 1)
  (smartparens-strict-mode 1)
  (rainbow-delimiters-mode 1))

;; lisp defaults
(add-hook 'clojure-mode-hook 'lisp-coding-defaults)
(add-hook 'lisp-mode-hook 'lisp-coding-defaults)
(add-hook 'emacs-lisp-mode-hook 'lisp-coding-defaults)
(add-hook 'cider-repl-mode-hook 'lisp-coding-defaults)

;; eldoc
(add-hook 'cider-clojure-interaction-mode-hook 'eldoc-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

;; cider configuration
(setq cider-repl-display-chelp-banner 'nil)

;; extra key bindings
(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "M-q") 'sp-indent-defun))

(provide 'wunki-lisp)
