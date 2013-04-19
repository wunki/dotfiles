;; set theme
(if (>= emacs-major-version 24)
  (progn
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
    (load-theme 'sanityinc-tomorrow-night))
  (progn
    (require 'color-theme)
    (color-theme-tomorrow-night)))

;; set the default font
(set-default-font "Ubuntu Mono-12")
(setq default-frame-alist '((font . "Ubuntu Mono 12")))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-solarized-light)
    (load-theme 'sanityinc-tomorrow-night)))

;; need this because italic was also underlined, no idea why...
(custom-set-faces
 '(italic ((t (:slant italic)))))

;;  '(mode-line-inactive ((t (:foreground "#ffffff" :background "#002451")))))
