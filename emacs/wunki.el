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
             paredit magit gist ack-and-a-half auto-complete jabber 
             buffer-move ido-ubiquitous s projectile goto-last-change
             expand-region powerline evil surround idomenu diminish
             dired-details
             ;; major modes
             org org-plus-contrib pandoc-mode rust-mode markdown-mode git-commit-mode
             haskell-mode clojurescript-mode
             ;; themes
             zenburn-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized))
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
(load "wunki/erlang")     ; erlang
(load "wunki/erc")        ; irc
(load "wunki/jabber")     ; jabber
(if (eq system-type 'darwin)
    (load "wunki/mac"))   ; mac settings

;; load email only on my local computer
(when (string-equal system-name "thinkpad.wunki.org")
  (load "wunki/mu4e"))
