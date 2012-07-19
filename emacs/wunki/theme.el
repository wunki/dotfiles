; some manually installed themes
(if (>= emacs-major-version 24)
  (progn
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
    (load-theme 'sanityinc-tomorrow-night))
  (progn
    (require 'color-theme)
    (color-theme-tomorrow-night)))

; set the default font
; fonts on the mac are rendered smaller.
(if (eq system-type 'darwin)
  (set-default-font "Ubuntu Mono-20")
  (set-default-font "Ubuntu Mono-14"))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-tomorrow-day)
    (load-theme 'sanityinc-tomorrow-night)))
(global-set-key (kbd "<f10>") 'toggle-dark-light-theme)
