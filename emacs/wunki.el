;; place to save customizations
(setq custom-file "~/.emacs.d/wunki/custom.el")

;; handy functions
(load "wunki/defuns")

;; load main configuration
(when (file-exists-p custom-file)
  (load custom-file))
(load "wunki/bindings")

;; packages
(when (not (require 'package nil t))
  (load-file "~/.emacs.d/package-23.el"))

(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(dolist (p '(nrepl ac-nrepl ac-slime clojure-mode clojure-test-mode cljdoc
             haskell-mode paredit magit gist org markdown-mode org org-plus-contrib
             ack-and-a-half markdown-mode color-theme-sanityinc-tomorrow
             zenburn-theme pandoc-mode auto-complete jabber clojurescript-mode
             buffer-move ido-ubiquitous s projectile goto-last-change
             expand-region git-commit-mode powerline evil surround idomenu
             rust-mode diminish))
  (when (not (package-installed-p p))
    (package-install p)))

(vendor 'slime)           ; older version of slime which works with swank
(vendor 'slime-repl)

;; configuration files
(load "wunki/modes")      ; settings for specific modes
(load "wunki/theme")      ; set the theme and font
(load "wunki/temp_files") ; temporary files
(load "wunki/org")        ; org-mode
(load "wunki/magit")      ; magit
(load "wunki/shell")      ; shell mode
(load "wunki/lisp")       ; lisp languages
(load "wunki/haskell")    ; haskell
(load "wunki/erc")        ; irc
(load "wunki/jabber")     ; jabber
(when (not (eq system-type 'berkeley-unix))
  (load "wunki/mu4e"))    ; load email on my local box
