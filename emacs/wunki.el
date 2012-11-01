; custom place to save customizations
(setq custom-file "~/.emacs.d/wunki/custom.el"
      marmalade-server "http://marmalade-repo.org/")

; my own functions
(load "wunki/defuns")

; load main custom file
(when (file-exists-p custom-file) (load custom-file))
(load "wunki/bindings")

; packages
(when (not (require 'package nil t))
  (load-file "~/.emacs.d/package-23.el"))

(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(dolist (p '(nrepl ac-nrepl clojure-mode clojure-test-mode cljdoc
             bbdb haskell-mode paredit magit gist org markdown-mode
             ack-and-a-half markdown-mode color-theme-sanityinc-tomorrow
             zenburn-theme pandoc-mode auto-complete jabber clojurescript-mode
             buffer-move ido-ubiquitous s projectile))
  (when (not (package-installed-p p))
    (package-install p)))

(load "wunki/modes")      ; settings for specific modes
(load "wunki/theme")      ; set the theme and font
(load "wunki/temp_files") ; temporary files
(load "wunki/org")        ; org-mode
(load "wunki/shell")      ; shell mode
(load "wunki/lisp")       ; lisp languages
(load "wunki/haskell")    ; haskell
(load "wunki/mu4e")       ; email
(load "wunki/erc")        ; irc
(load "wunki/jabber")     ; jabberj
