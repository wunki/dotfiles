;; place to save customizations
(setq custom-file "~/.emacs.d/wunki/wunki-custom.el")
(require 'cl)

;; load main configuration
(when (file-exists-p custom-file)
  (load custom-file))

;; package repositories
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
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
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    js2-mode
    yaml-mode
    ido-ubiquitous
    flx-ido
    undo-tree
    ace-jump-mode
    web-mode
    anzu
    fish-mode
    company
    company-ghc
    cider
    clojure-mode
    clj-refactor
    cljdoc
    clojurescript-mode
    haskell-mode
    shm
    ghc
    anaconda-mode
    toml-mode
    rust-mode
    flycheck-rust
    racket-mode
    go-mode
    go-eldoc
    gotest
    go-projectile
    company-go
    golint
    zenburn-theme
    color-theme-sanityinc-tomorrow
    color-theme-sanityinc-solarized
    nginx-mode))

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
(when (mac?) (require 'wunki-mac)) ; mac settings
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

;; email only on my local computer
(when (or (string-equal system-name "home.wunki.org")
          (string-equal system-name "macbook.wunki.org"))
  (require 'wunki-mu4e))
