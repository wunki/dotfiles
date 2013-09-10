;; lisp settings for Clojure

;; colors for parenthesis
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; paredit everywhere
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(add-hook 'nrepl-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; lisp implementation is SBCL
(setq inferior-lisp-program "/usr/bin/sbcl --noinform")

;; nrepl
(setq nrepl-port "4001")

;; eldoc
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; slime
(slime-setup '(slime-repl))

;; slime auto-completion
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; nrepl autocompletion
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

(defun nrepl-connection-infos (connection-buffer)
  (with-current-buffer (get-buffer connection-buffer)
    nrepl-endpoint))

(defun nrepl-current-connection-infos ()
  (nrepl-connection-infos (nrepl-current-connection-buffer)))

(defun nrepl-rotate-connection-list (connection-list)
  (append (rest connection-list)
          (list (first connection-list))))

(defun nrepl-show-current-connection ()
  (apply #'message
         "Active nREPL connection: %s:%s"
         (nrepl-current-connection-infos)))

(defun nrepl-switch-to-next-connection ()
  (interactive)
  (setq nrepl-connection-list
        (nrepl-rotate-connection-list nrepl-connection-list))
  (nrepl-show-current-connection))

;; bindings
(global-set-key (kbd "C-c C-x n") 'nrepl-switch-to-next-connection)

(provide 'wunki-lisp)
