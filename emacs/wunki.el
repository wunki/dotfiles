; third party plugins
(add-to-list 'load-path "~/.emacs.d/vendor")

; custom place to save customizations
(setq custom-file "~/.emacs.d/wunki/custom.el")
(when (file-exists-p custom-file) (load custom-file))

(load "wunki/defuns")
(load "wunki/theme")
(load "wunki/temp_files")

(vendor 'magit)
