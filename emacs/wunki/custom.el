;; Customization file. This file contains all my personal preferences.
(prefer-coding-system 'utf-8)
(setq-default tab-width 2 indent-tabs-mode nil)
(column-number-mode 1) ;; show column numbers
(setq user-mail-address "petar@wunki.org")

;; flash instead of bell
(setq visible-bell t)

;; disable scrollbars
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Always show the region
(setq transient-mark-mode t)

;; Format the title-bar to always include the buffer name
(setq frame-title-format " %b (%m)")

;; window movement
(windmove-default-keybindings 'meta)

;; autoselect window with mouse
(setq mouse-autoselect-window t)

;; Dont show the GNU splash screen
(setq inhibit-startup-message t)

;; Dont ask for yes or no, just use y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Map execute command to the C-key. Less hand movement with this command.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Kill word and avoid using the backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; temporary disable arrow keys so I'm forced to learn emacs movements
(global-unset-key [(up)])
(global-unset-key [(down)])
(global-unset-key [(left)])
(global-unset-key [(right)])
(global-unset-key "\C-x\C-b")

;; hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

;; auto-fill-mode settings
(setq-default fill-column 78)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; truncate long lines
(setq-default truncate-lines nil)
