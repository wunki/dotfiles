; some manually installed themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

; solarized theme requires common-lisp
(require 'cl)

; default theme
(load-theme 'sanityinc-solarized-dark)

; set the default font
; fonts on the mac are rendered smaller.
(if (eq system-type 'darwin)
  (set-default-font "Consolas-19")
  (set-default-font "Consolas-15"))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-solarized-light)
    (load-theme 'sanityinc-solarized-dark)))
(global-set-key (kbd "<f8>") 'toggle-dark-light-theme)
