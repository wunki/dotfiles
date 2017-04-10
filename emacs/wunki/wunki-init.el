;; place to save customizations
(setq custom-file "~/.emacs.d/wunki/wunki-custom.el")
(require 'cl)

;; load main configuration
(when (file-exists-p custom-file)
  (load custom-file))

;; my packages
(defvar wunki-packages
  '(projectile           ; project manegement
    expand-region        ; easily select regions
    spaceline            ; pretty modeline
    diminish             ; remove modes from the modeline
    dired-details        ; show details in dired
    multiple-cursors     ; multiple cursors
    ag                   ; quickly find stuff
    rainbow-delimiters   ; color matching parenthesis
    smex                 ; find recently found commands
    fullframe            ; enable full frames for modes
    smartparens          ; easy lisp editing
    move-text            ; move line up/down
    browse-kill-ring     ; show the kill ring
    exec-path-from-shell ; use env vars in emacs
    flycheck             ; linter
    ido-ubiquitous       ; use ido everywhere
    flx-ido              ; matching engine for ido
    undo-tree            ; show undo's in a tree
    avy                  ; jump around inside a buffer
    ace-window           ; switch between emacs windows
    anzu                 ; display current and total matches
    company              ; autocomplete engine
    default-text-scale   ; easily scale text
    fic-mode             ; highlight fixme, todo etc.
    twittering-mode      ; twitter client
    
    ;; file modes
    js2-mode
    web-mode
    yaml-mode
    markdown-mode
    fish-mode
    csharp-mode
    erlang
    google-c-style
    nginx-mode
    dockerfile-mode

    ;; git
    magit
    eshell-git-prompt
    gitconfig-mode
    gitignore-mode
    
    ;; org
    org
    org-plus-contrib
    org-magit
    ox-gfm
    
    ;; package management
    async
    paradox

    ;; elixir
    alchemist

    ;; rust
    rust-mode
    racer
    toml-mode
    company-racer
    flycheck-rust

    ;; colors
    zenburn-theme
    color-theme-sanityinc-tomorrow    
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
(require 'wunki-eshell)            ; eshell settings
(require 'wunki-html)              ; HTML files
(require 'wunki-lisp)              ; lisp languages
(require 'wunki-scheme)            ; scheme languages
(require 'wunki-rust)              ; rust
(require 'wunki-python)            ; python
(require 'wunki-erlang)            ; erlang
(require 'wunki-elixir)            ; elixir
(require 'wunki-csharp)            ; c#
(require 'wunki-go)                ; go
(require 'wunki-c)                 ; c
(require 'wunki-erc)               ; irc
(when (file-exists-p "~/mail")     ; mu4e
  (require 'wunki-mu4e))
