;; requirements
(require 'haskell-mode)

;; ghc-mod
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; customization
(custom-set-variables
 '(haskell-process-type 'cabal-repl)
 '(haskell-notify-p nil)
 '(haskell-tags-on-save t)
 '(haskell-stylish-on-save nil))

;; hooks
(add-hook 'haskell-mode-hook 'haskell-hook)
(add-hook 'haskell-interactive-mode-hook 'haskell-interactive-hook)
(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;; haskell mode bindings
(defun haskell-hook ()
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (define-key haskell-mode-map [f5] 'haskell-process-load-or-reload)
  (define-key haskell-mode-map [f12] 'haskell-process-cabal-build-and-restart)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)
  (define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal))

(defun haskell-cabal-hook ()
  (define-key haskell-cabal-mode-map [f9] 'haskell-interactive-mode-visit-error)
  (define-key haskell-cabal-mode-map [f11] 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map [f12] 'haskell-process-cabal-build-and-restart)
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear))

(defun haskell-interactive-hook ()
  (define-key haskell-interactive-mode-map [f9] 'haskell-interactive-mode-visit-error)
  (define-key haskell-interactive-mode-map [f11] 'haskell-process-cabal-build)
  (define-key haskell-interactive-mode-map [f12] 'haskell-process-cabal-build-and-restart)
  (define-key haskell-interactive-mode-map (kbd "C-c C-v") 'haskell-interactive-toggle-print-mode)
  (define-key haskell-interactive-mode-map (kbd "C-c C-p") 'haskell-interactive-mode-error-backward)
  (define-key haskell-interactive-mode-map (kbd "C-c C-n") 'haskell-interactive-mode-error-forward)
  (define-key haskell-interactive-mode-map (kbd "C-c c") 'haskell-process-cabal))

(provide 'wunki-haskell)
