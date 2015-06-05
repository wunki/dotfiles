;; get the correct exec-path. I thought this was only needed on my Mac, but
;; apparently it also fails to the correct path on Linux, maybe due to my zsh
;; shell.
(exec-path-from-shell-initialize)

;; use fish as the default shell
(setq explicit-shell-file-name (locate-file "fish" exec-path))

;; copy some vars over
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "PYTHONPATH")

;; needed modes
(add-hook 'term-mode-hook
          (lambda ()
            (goto-address-mode)          ; ability to click on links
            (toggle-truncate-lines)))

;; close the buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; don't ask which shell to use, just start fish
(defadvice ansi-term (before force-fish)
  (interactive (list explicit-shell-file-name)))
(ad-activate 'ansi-term)

;; use utf-8 in terminal
(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)

(provide 'wunki-shell)
