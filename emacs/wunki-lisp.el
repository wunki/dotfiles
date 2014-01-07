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

;; lisp implementation is SBCL
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")

;; slime
(slime-setup '(slime-repl))
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; cider
(setq nrepl-port "4001")
(setq nrepl-buffer-name-show-port t)
(setq nrepl-hide-special-buffers nil)
(setq cider-auto-select-error-buffer t)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; use tab to trigger auto-complete
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; key bindings
(eval-after-load 'cider-mode
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "M-q") 'sp-indent-defun))

(provide 'wunki-lisp)
