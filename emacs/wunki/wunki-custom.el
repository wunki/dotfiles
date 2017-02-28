;; default tab-width is two spaces
(setq-default tab-width 2
              js-indent-level 2
              c-basic-offset 2
              indent-tabs-mode nil)

;; email
(setq user-full-name "Petar Radosevic")
(setq user-mail-address "petar@wunki.org")

;; chrome as browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; desktop, to automatically save and restore sessions
(setq desktop-dirname             "~/.emacs.d/desktop/"
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)

;; don't show any scratch message
(setq initial-scratch-message nil)

;; unicode
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; warnings
(setq visible-bell t)
(setq ring-bell-function (lambda nil (message "")))

;; scrolling
(setq scroll-conservatively 10000) ; don't jump my screen!

;; autosave
(setq auto-save-interval 500)

;; follow symlinks and don't ask questions
(setq vc-follow-symlinks t)

;; always show the region
(setq transient-mark-mode t)

;; enable clipboard on x
(setq x-select-enable-clipboard t)

;; format the title-bar to always include the buffer name
(setq frame-title-format " %b (%m)")

;; autoselect window with mouse
(setq mouse-autoselect-window t)

;; dont show the GNU splash screen
(setq inhibit-startup-message t)

;; dont ask for yes or no, just use y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; auto-fill
(setq-default fill-column 78)

;; truncate long lines
(setq-default truncate-lines nil)

;; european dates
(setq calendar-date-style 'european)

;; prevents warning when sending mail
(setq gnutls-min-prime-bits 1024)

;; spelling
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; set by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "0f6667c7dd4d19cf99cde5feeb6f0fe6abebc229282b5e21d1739b1fe14b5342" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(haskell-notify-p t)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-stylish-on-save t t)
 '(haskell-tags-on-save t t)
 '(org-agenda-files
   (quote
    ("/Users/wunki/Stack/Org/inbox.org" "/Users/wunki/Stack/Org/today.org" "/Users/wunki/Stack/Org/servers.org")))
 '(package-selected-packages
   (quote
    (dockerfile-mode rust-mode haskell-mode cider yaml-mode company avy flycheck multiple-cursors expand-region projectile magit alchemist zenburn-theme yagist web-mode undo-tree twittering-mode toml-mode tao-theme surround spaceline solarized-theme smex smartparens salt-mode rustfmt rainbow-delimiters racer pyenv-mode-auto paradox pandoc-mode org-pomodoro org-plus-contrib org-magit nginx-mode move-text markdown-mode js2-mode intero idomenu ido-ubiquitous hindent gruvbox-theme gotest google-c-style golint go-projectile gitignore-mode gitconfig-mode fullframe flycheck-rust flycheck-hdevtools flx-ido fish-mode fic-mode exec-path-from-shell eshell-git-prompt erlang dired-details diminish default-text-scale dash-at-point darktooth-theme csharp-mode company-racer company-go company-ghc company-anaconda color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clojurescript-mode cljdoc clj-refactor change-inner buffer-move browse-kill-ring anzu ag ack-and-a-half ace-window)))
 '(paradox-github-token t)
 '(python-shell-interpreter "python2")
 '(safe-local-variable-values
   (quote
    ((indent-tabs-mode . 1)
     (pony-settings make-pony-project :python "/home/wunki/.virtualenvs/mijnmazda/bin/python"))))
 '(smtpmail-smtp-server "mail.messagingengine.com")
 '(smtpmail-smtp-service 587))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(erc-prompt-face ((t (:background nil))))
 '(italic ((t (:slant italic)))))
