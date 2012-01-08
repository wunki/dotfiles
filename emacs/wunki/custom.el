; customization file, containing personal preferences.
(setq-default tab-width 2 indent-tabs-mode nil)
(column-number-mode 1) ;; show column numbers
(setq user-mail-address "petar@wunki.org")

; Unicode
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; flash instead of bell
; (setq visible-bell t)
(setq ring-bell-function (lambda nil (message "")))

; some packages require common lisp (solarized!)
(require 'cl)

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
 '(custom-safe-themes (quote ("1440d751f5ef51f9245f8910113daee99848e2c0" "485737acc3bedc0318a567f1c0f5e7ed2dfde3fb" "d14db41612953d22506af16ef7a23c4d112150e5" "7fe1e3de3e04afc43f9a3d3a8d38cd0a0efd9d4c" "04fd52af504d80a42d9487e3e6aa96b6937255d1" "1f392dc4316da3e648c6dc0f4aad1a87d4be556c" "5600dc0bb4a2b72a613175da54edb4ad770105aa" "0174d99a8f1fdc506fa54403317072982656f127" default)))
 '(safe-local-variable-values (quote ((pony-settings make-pony-project :python "/Users/wunki/.virtualenvs/invy-promo/bin/python" :settings "settings") (pony-settings make-pony-project :python "/Users/wunki/.virtualenvs/invy-web/bin/python")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
