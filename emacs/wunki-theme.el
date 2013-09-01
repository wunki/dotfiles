;; active theme
(load-theme 'zenburn)

;; font
(set-frame-font "Ubuntu Mono-12")
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-solarized-light)
    (load-theme 'zenburn)))

(provide 'wunki-theme)
