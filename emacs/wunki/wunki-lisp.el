;; lisp languages (mainly clojure)

;; enable this on lisp languages
(defun lisp-coding-defaults ()
  (subword-mode 1)
  (smartparens-strict-mode 1)
  (rainbow-delimiters-mode 1))

;; colors for parenthesis
(add-hook 'clojure-mode-hook 'lisp-coding-defaults)
(add-hook 'lisp-mode-hook 'lisp-coding-defaults)
(add-hook 'emacs-lisp-mode-hook 'lisp-coding-defaults)
(add-hook 'cider-repl-mode-hook 'lisp-coding-defaults)

;; cleanup buffer to cleanup functions
(require 'clj-refactor)
(add-to-list 'cljr-project-clean-functions 'cleanup-buffer)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-m")))

;; lisp implementation is SBCL
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")

;; cider
(setq nrepl-port "4001")
(setq nrepl-buffer-name-show-port t)
(setq cider-auto-select-error-buffer t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)

;; key bindings
(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "M-q") 'sp-indent-defun))

(provide 'wunki-lisp)
