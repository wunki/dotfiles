(require 'color-theme)
(setq color-theme-is-global t)

(load-file "~/.emacs.d/elpa/color-theme-ir-black-1.0.1/color-theme-ir-black.elc")
(load-file "~/.emacs.d/elpa/color-theme-github-0.0.3/color-theme-github.elc")
(load-file "~/.emacs.d/elpa/color-theme-dpaste-0.0.1/color-theme-dpaste.elc")
(load-file "~/.emacs.d/elpa/color-theme-zenburn-0.3/color-theme-zenburn.el")

; select the colorscheme
(color-theme-twilight)

; set the default font
(set-default-font "Menlo-16")
