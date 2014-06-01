;; load theme and don't ask if it's save.
(load-theme 'sanityinc-tomorrow-night t)

;; font -- some strange issues with font size, 110 is huge, 100 is tiny.
(set-frame-font "Fira Mono")
(add-to-list 'default-frame-alist '(font . "Fira Mono"))
(set-face-attribute 'default nil :height 105)

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-solarized-light)
    (load-theme 'zenburn)))

(provide 'wunki-theme)
