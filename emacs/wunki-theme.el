;; active theme
(load-theme 'zenburn)

;; font
(set-frame-font "DejaVu Sans Mono-10.5")
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10.5"))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-solarized-light)
    (load-theme 'zenburn)))

(provide 'wunki-theme)
