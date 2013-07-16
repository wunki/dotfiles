;; default tab-width is two spaces
(setq-default tab-width 2
              js-indent-level 2
              c-basic-offset 2
              indent-tabs-mode nil)

;; email
(setq user-full-name "Petar Radosevic")
(setq user-mail-address "petar@wunki.org")

;; chromium as browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "~/bin/conkeror")

;; set the editor hardcoded to Emacs. Fixes Magit.
(setenv "EDITOR" "emacsclient")

;; unicode
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
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
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; truncate long lines
(setq-default truncate-lines nil)

;; european dates
(setq calendar-date-style 'european)

;; prevents warning when sending mail
(setq gnutls-min-prime-bits 1024)

;; spelling
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; call me crazy but I only want lowercase on my linux system
(setq dropbox-directory (if (string-equal system-type "gnu/linux")
                            (expand-file-name "~/dropbox")
                          (expand-file-name "~/Dropbox")))

;; disable scrollbars and menu bar on the mac
;; disable it with .Xdefaults in linux
(when (string-equal system-type "darwin")
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

;; set by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(haskell-notify-p t)
 '(haskell-process-type (quote cabal-dev))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-prompt-face ((t (:foreground "#81a2be" :weight bold))))
 '(italic ((t (:slant italic)))))
