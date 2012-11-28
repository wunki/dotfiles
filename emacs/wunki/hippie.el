;; Hippie expand stuff stolen from Moritz
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name))

(defun indent-or-hippie-expand ()
  "Complete if point is at end of a word, indent otherwise."
  (interactive)
  (if (looking-at "\\_>")
      ;; skip message output
      (flet ((message (format-string &rest args) nil))
        (hippie-expand nil))
    ;; otherwise indent line
    (indent-for-tab-command)))

(defun indent-and-hippie-expand ()
  "Complete if point is at the end of a word and indent."
  (interactive)
  (if (looking-at "\\_>")
      ;; skip message output
      (flet ((message (format-string &rest args) nil))
        (hippie-expand nil)))
  ;; always indent line
  (indent-for-tab-command))

(define-key (current-global-map) (kbd "TAB") 'indent-and-hippie-expand)
(define-key (current-global-map) (kbd "C-<tab>") 'hippie-expand)
