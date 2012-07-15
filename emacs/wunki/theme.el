; some manually installed themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

; default theme
(load-theme 'sanityinc-tomorrow-night)

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
