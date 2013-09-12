(require 'magit)

;; Add an extra newline to separate commit message from git commentary
(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1)))
(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)

;; close popup when commiting
(defadvice git-commit-commit (after delete-window activate)
  (delete-window))

(provide 'wunki-magit)
