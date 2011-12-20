;(load-file "~/.emacs.d/elpa/color-theme-github-0.0.3/color-theme-github.elc")
;(load-file "~/.emacs.d/elpa/color-theme-wombat+-0.0.2/color-theme-wombat+.elc")
;(load-file "~/.emacs.d/elpa/color-theme-zenburn-0.3/color-theme-zenburn.el")
;(load-file "~/.emacs.d/elpa/color-theme-railscasts-0.0.2/color-theme-railscasts.elc")
;(load-file "~/.emacs.d/elpa/color-theme-ir-black-1.0.1/color-theme-ir-black.elc")
;(load-file "~/.emacs.d/elpa/color-theme-twilight-0.1/color-theme-twilight.elc")

; color theme is global
(setq color-theme-is-global t)

; select the colorscheme
(color-theme-solarized-dark)

; set the default font
; fonts on the mac are rendered smaller.
(if (eq system-type 'darwin)
  (set-default-font "Consolas-20")
  (set-default-font "Consolas-15"))

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (color-theme-solarized-light)
    (color-theme-solarized-dark)))
(global-set-key (kbd "<f8>") 'toggle-dark-light-theme)
