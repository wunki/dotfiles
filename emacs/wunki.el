; custom place to save customizations
(setq custom-file "~/.emacs.d/wunki/custom.el"
      marmalade-server "http://marmalade-repo.org/")

(when (file-exists-p custom-file) (load custom-file))

; bindings are important, load them first
(load "wunki/bindings")

; packages
(when (not (require 'package nil t))
  (load-file "~/.emacs.d/package-23.el"))

(add-to-list 'package-archives
	           '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(dolist (p '(slime slime-repl clojure-mode clojure-test-mode
             paredit magit gist deft markdown-mode ack-and-a-half
             color-theme-sanityinc-tomorrow zenburn-theme))
  (when (not (package-installed-p p))
    (package-install p)))

(load "wunki/defuns")

(vendor 'markdown-mode)
(vendor 'puppet-mode)
(vendor 'pandoc-mode)
(when (< emacs-major-version 24)
  (vendor 'color-theme-tomorrow))
(vendor 'hsenv)
(vendor 'virtualenv)

(load "wunki/modes")
(load "wunki/theme")
(load "wunki/temp_files")
(load "wunki/org")
(load "wunki/shell")
(load "wunki/haskell")
(load "wunki/gnus")
(load "wunki/erc")
