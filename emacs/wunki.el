; third party plugins
(add-to-list 'load-path "~/.emacs.d/vendor")

; custom place to save customizations
(setq custom-file "~/.emacs.d/wunki/custom.el")
(when (file-exists-p custom-file) (load custom-file))

; packages with the help of marmalade
(require 'package)
(add-to-list 'package-archives
	           '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(load "wunki/defuns")
(load "wunki/bindings")
(load "wunki/modes")
(load "wunki/theme")
(load "wunki/temp_files")
