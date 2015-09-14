;; place to save customizations
(setq custom-file "~/.emacs.d/wunki/wunki-custom.el")
(require 'cl)

;; load main configuration
(when (file-exists-p custom-file)
  (load custom-file))

;; package repositories
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; my packages
(defvar wunki-packages
  '(magit
    yagist
    ack-and-a-half
    buffer-move
    s
    projectile
    goto-last-change
    expand-region
    change-inner
    powerline
    surround
    idomenu
    diminish
    dired-details
    multiple-cursors
    ag
    rainbow-delimiters
    smex
    fullframe
    smartparens
    move-text
    dash
    browse-kill-ring
    exec-path-from-shell
    flycheck
    dash-at-point
    twittering-mode
    org
    org-plus-contrib
    org-magit
    pandoc-mode
    markdown-mode
    gitconfig-mode
    gitignore-mode
    js2-mode
    yaml-mode
    ido-ubiquitous
    flx-ido
    undo-tree
    avy
    ace-window
    web-mode
    anzu
    fish-mode
    company
    default-text-scale

    ;; clojure
    cider
    clojure-mode
    clj-refactor
    cljdoc
    clojurescript-mode

    ; packages
    paradox
    async
    
    ; haskell
    haskell-mode
    flycheck-haskell
    company-ghc

    ; rust
    rust-mode
    racer
    toml-mode
    company-racer
    flycheck-rust

    ; python
    anaconda-mode
    pyenv-mode
    
    ;; erlang
    erlang

    ;; go
    go-mode
    go-eldoc
    gotest
    go-projectile
    company-go
    golint

    ;; colors
    zenburn-theme
    solarized-theme
    gruvbox-theme
    color-theme-sanityinc-tomorrow
    color-theme-sanityinc-solarized

    ;; elixir
    elixir-mode
    alchemist

    ;; general
    nginx-mode
    ))

;; install packages if not there yet, copied from prelude
(defun wunki-packages-installed-p ()
  "Check if all packages in `wunki-packages' are installed."
  (every #'package-installed-p wunki-packages))

(defun wunki-require-package (package)
  "Install PACKAGE unless already installed."
  (unless (memq package wunki-packages)
    (add-to-list 'wunki-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun wunki-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'wunki-require-package packages))

(defun wunki-install-packages ()
  "Install all packages listed in `wunki-packages'."
  (unless (wunki-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (wunki-require-packages wunki-packages)))

;; run package installation
(wunki-install-packages)

;; configuration files
(setq config-dir (file-name-directory
                  (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path config-dir)

(require 'wunki-defuns)            ; my functions
(require 'wunki-modes)             ; settings for specific modes
(require 'wunki-bindings)          ; load bindings
(require 'wunki-theme)             ; set the theme and font
(when (mac?)
    (require 'wunki-mac))          ; mac settings
(require 'wunki-temp)              ; temporary files
(require 'wunki-git)               ; (ma)git settings
(require 'wunki-org)               ; org-mode
(require 'wunki-shell)             ; shell mode
(require 'wunki-html)              ; HTML files
(require 'wunki-lisp)              ; lisp languages
(require 'wunki-scheme)            ; scheme languages
(require 'wunki-rust)              ; rust
(require 'wunki-python)            ; python
(require 'wunki-haskell)           ; haskell
(require 'wunki-erlang)            ; erlang
(require 'wunki-go)                ; go
(require 'wunki-erc)               ; irc
(when (file-exists-p "~/mail")     ; mu4e
  (require 'wunki-mu4e))
