(require 'magit)

;; close popup when commiting
(defadvice git-commit-commit (after delete-window activate)
  (delete-window))

(provide 'wunki-magit)
