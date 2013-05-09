;; themes and fonts
(if (>= emacs-major-version 24)
  (progn
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
    (load-theme 'sanityinc-tomorrow-night))
  (progn
    (require 'color-theme)
    (color-theme-tomorrow-blue)))

;; font
(set-frame-font "Ubuntu Mono")
(set-face-attribute 'default nil :height 105)

;; need this because italic was also underlined, no idea why...
(custom-set-faces
 '(italic ((t (:slant italic)))))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-solarized-light)
    (load-theme 'sanityinc-tomorrow-night)))
