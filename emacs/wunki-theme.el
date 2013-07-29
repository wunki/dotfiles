;; active theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'sanityinc-tomorrow-night)

;; fonts
(set-frame-font "Ubuntu Mono-12")
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-12"))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-solarized-light)
    (load-theme 'sanityinc-tomorrow-night)))

(provide 'wunki-theme)
