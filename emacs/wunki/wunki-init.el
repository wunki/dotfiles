;; place to save customizations
(setq custom-file "~/.emacs.d/wunki/wunki-custom.el")
(require 'cl)

;; load main configuration
(when (file-exists-p custom-file)
  (load custom-file))

;; my packages
(defvar wunki-packages
  '(magit
    yagist
    ack-and-a-half
    buffer-move
    projectile
    expand-region
    change-inner
    spaceline
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
    pandoc-mode
    markdown-mode
    gitconfig-mode
    gitignore-mode
    js2-mode
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
    fic-mode

    ;; org
    org
    org-plus-contrib
    org-magit
    org-pomodoro

    ;; server management
    yaml-mode
    salt-mode
    
    ;; package management
    async
    paradox

    ;; clojure
    cider
    clojure-mode
    clj-refactor
    cljdoc
    clojurescript-mode

    ; haskell
    haskell-mode
    flycheck-hdevtools
    company-ghc
    intero

    ; rust
    rust-mode
    racer
    toml-mode
    company-racer
    flycheck-rust
    rustfmt

    ; python
    anaconda-mode
    company-anaconda
    pyenv-mode
    pyenv-mode-auto
    
    ;; erlang
    erlang

    ;; c#
    csharp-mode

    ;; go
    go-mode
    go-eldoc
    gotest
    go-projectile
    company-go
    golint

    ;; colors
    zenburn-theme
    tao-theme
    solarized-theme
    gruvbox-theme
    color-theme-sanityinc-tomorrow
    color-theme-sanityinc-solarized

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
(if (not (windows?))
    (require 'wunki-term))         ; ansi-term settings
(require 'wunki-html)              ; HTML files
(require 'wunki-lisp)              ; lisp languages
(require 'wunki-scheme)            ; scheme languages
(require 'wunki-rust)              ; rust
(require 'wunki-python)            ; python
(require 'wunki-haskell)           ; haskell
(require 'wunki-erlang)            ; erlang
(require 'wunki-elm)               ; elm
(require 'wunki-csharp)            ; c#
(require 'wunki-go)                ; go
(require 'wunki-erc)               ; irc
(require 'wunki-yaml)              ; yaml
(when (file-exists-p "~/mail")     ; mu4e
  (require 'wunki-mu4e))
