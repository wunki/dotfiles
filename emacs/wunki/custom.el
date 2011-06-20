; customization file, containing personal preferences.
(prefer-coding-system 'utf-8)
(setq-default tab-width 4 indent-tabs-mode nil)
(column-number-mode 1) ;; show column numbers
(setq user-mail-address "petar@wunki.org")

; flash instead of bell
(setq visible-bell t)

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
(server-start)

; ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)