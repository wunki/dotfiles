(require 'color-theme)
(setq color-theme-is-global t)

(load-file "~/.emacs.d/elpa/color-theme-zenburn-0.3/color-theme-zenburn.el")
(load-file "~/.emacs.d/elpa/color-theme-solarized-1.0.0/color-theme-solarized.elc")

; select the colorscheme
(color-theme-solarized-dark)

; set the default font
(set-default-font "Menlo-15")
