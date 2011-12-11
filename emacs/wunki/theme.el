(load-file "~/.emacs.d/elpa/color-theme-github-0.0.3/color-theme-github.elc")
;(load-file "~/.emacs.d/elpa/color-theme-wombat+-0.0.2/color-theme-wombat+.elc")
;(load-file "~/.emacs.d/elpa/color-theme-zenburn-0.3/color-theme-zenburn.el")
;(load-file "~/.emacs.d/elpa/color-theme-railscasts-0.0.2/color-theme-railscasts.elc")
;(load-file "~/.emacs.d/elpa/color-theme-ir-black-1.0.1/color-theme-ir-black.elc")

(load-file "~/.emacs.d/elpa/color-theme-twilight-0.1/color-theme-twilight.elc")

; color theme is global
(setq color-theme-is-global t)

; select the colorscheme
(color-theme-twilight)

; set the default font
; fonts on the mac are a bit smaller
(if (eq system-type 'darwin)
  (set-default-font "Consolas-19")
  (set-default-font "Consolas-16"))
