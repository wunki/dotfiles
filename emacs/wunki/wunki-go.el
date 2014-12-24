;; flycheck

(if (mac?)
    (add-to-list 'load-path "~/Go/src/github.com/dougm/goflymake")
  (add-to-list 'load-path "~/go/src/github.com/dougm/goflymake"))
(require 'go-flycheck)

;; auto-completion
(require 'company-go)
(require 'go-projectile)

(add-hook 'go-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-go))
            (setq gofmt-command "goimports")
            (if (not (string-match "go" compile-command))
                (set (make-local-variable 'compile-command)
                     "go build -v; go test -v; go vet"))
            (flycheck-mode)
            (company-mode)))

;; documentation
(add-hook 'go-mode-hook 'go-eldoc-setup)

;; configuration
(add-hook 'go-mode-hook
          '(lambda()
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             (local-set-key (kbd "M-.") 'godef-jump)
             (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
             (local-set-key (kbd "C-c i") 'go-goto-imports)
             (local-set-key (kbd "C-c d") 'godoc)))

(define-key go-mode-map (kbd "C-x f") 'go-test-current-file)
(define-key go-mode-map (kbd "C-x t") 'go-test-current-test)
(define-key go-mode-map (kbd "C-x p") 'go-test-current-project)
(define-key go-mode-map (kbd "C-x x") 'go-run)

(add-hook 'before-save-hook 'gofmt-before-save)

(provide 'wunki-go)
