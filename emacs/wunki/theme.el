; some manually installed themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

; default theme
(load-theme 'wombat)

; set the default font
; fonts on the mac are rendered smaller.
(if (eq system-type 'darwin)
  (set-default-font "Consolas-19")
  (set-default-font "Consolas-15"))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'solarized-light)
    (load-theme 'solarized-dark)))
(global-set-key (kbd "<f8>") 'toggle-dark-light-theme)
