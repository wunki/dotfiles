;; use fish as the default shell
(setq explicit-shell-file-name "/usr/local/bin/fish")

;; needed modes
(add-hook 'term-mode-hook
          (lambda ()
            (goto-address-mode)          ; ability to click on links
            (toggle-truncate-lines)))

;; don't ask which shell to use, just start fish
(defvar my-term-shell "/usr/local/bin/fish")
(defadvice ansi-term (before force-fish)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; use utf-8 in terminal
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

(provide 'wunki-shell)
