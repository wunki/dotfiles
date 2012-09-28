; some manually installed themes
(if (>= emacs-major-version 24)
  (progn
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
    (load-theme 'sanityinc-tomorrow-blue))
  (progn
    (require 'color-theme)
    (color-theme-tomorrow-night)))

; set the default font
; fonts on the mac are rendered smaller.
(if (eq system-type 'darwin)
  (set-default-font "Droid Sans Mono-16")
  (set-default-font "Consolas-12"))

(defun toggle-dark-light-theme-tomorrow ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-tomorrow-day)
    (load-theme 'sanityinc-tomorrow-night)))

(defun toggle-dark-light-theme-solarized ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-solarized-light)
    (load-theme 'sanityinc-solarized-dark)))
(global-set-key (kbd "<f10>") 'toggle-dark-light-theme-solarized)

; needed this because italic was also underlined, no idea why...
(custom-set-faces
 '(italic ((t (:slant italic)))))
