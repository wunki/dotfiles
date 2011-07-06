; custom place to save customizations
(setq custom-file "~/.emacs.d/wunki/custom.el"
      marmalade-server "http://marmalade-repo.org/")

(when (file-exists-p custom-file) (load custom-file))

; packages
(when (not (require 'package nil t))
  (load-file "~/.emacs.d/package-23.el"))

(add-to-list 'package-archives
	           '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(dolist (p '(clojure-mode magit color-theme-zenburn gist))
  (when (not (package-installed-p p))
    (package-install p)))

(load "wunki/defuns")

(vendor 'markdown-mode)
(vendor 'puppet-mode)

(load "wunki/bindings")
(load "wunki/modes")
(load "wunki/theme")
(load "wunki/temp_files")


