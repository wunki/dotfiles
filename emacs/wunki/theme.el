(load-file "~/.emacs.d/vendor/solarized/emacs-colors-solarized/color-theme-solarized.el")

; color theme is global
(setq color-theme-is-global t)

; select the colorscheme
(color-theme-twilight)

; set the default font
; fonts on the mac are a bit smaller
(if (eq system-type 'darwin)
  (set-default-font "Inconsolata-g-16")
  (set-default-font "Inconsolata-g-13"))