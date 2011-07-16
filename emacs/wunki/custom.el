; customization file, containing personal preferences.
(prefer-coding-system 'utf-8)
(setq-default tab-width 2 indent-tabs-mode nil)
(column-number-mode 1) ;; show column numbers
(setq user-mail-address "petar@wunki.org")

; Unicode
(prefer-coding-system 'utf-8)

; flash instead of bell
(setq visible-bell t)

; don't blink the cursor
(blink-cursor-mode -1)

; add bin directory from homebrew
(push "/usr/local/bin" exec-path)

; disable scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

; always show the region
(setq transient-mark-mode t)

; format the title-bar to always include the buffer name
(setq frame-title-format " %b (%m)")

; window movement
(windmove-default-keybindings 'meta)

; autoselect window with mouse
(setq mouse-autoselect-window t)

; dont show the GNU splash screen
(setq inhibit-startup-message t)

; dont ask for yes or no, just use y or n
(fset 'yes-or-no-p 'y-or-n-p)

; auto-fill-mode settings
(setq-default fill-column 78)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; truncate long lines
(setq-default truncate-lines nil)

; run the emacs server
(load "server")
(unless (server-running-p) (server-start))

; ido-mode
(ido-mode 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10)

; european dates
(setq calendar-date-style 'european)

; spelling
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sendmail-query-once-function (quote sendmail-send-it) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
