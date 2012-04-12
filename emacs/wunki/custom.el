; customization file, containing personal preferences.
(setq-default tab-width 2 indent-tabs-mode nil)
(column-number-mode 1) ;; show column numbers

; email settings
(setq user-full-name "Petar Radosevic")
(setq user-mail-address "petar@wunki.org")

; Unicode
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; flash instead of bell
; (setq visible-bell t)
(setq ring-bell-function (lambda nil (message "")))

; don't blink the cursor
(blink-cursor-mode -1)

; add bin directory from homebrew and cabal
(push "/usr/local/bin" exec-path)
(push "~/.cabal/bin" exec-path)

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

; ack aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

; save that theme is safe
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" "965234e8069974a8b8c83e865e331e4f53ab9e74" "b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1056c413dd792adddc4dec20e8c9cf1907e164ae" "b0950b032aa3c8faab4864ae288296dd66b92eca" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "39327baac0e924fc06c561986ed6fff862df8e1d" "a677b91598bc31bc159847fa2a3c0f525c7bf5bb" "69546801bd9c98eeb7246a3d39497abeced1d11e" "bf5b179a9e82b3e818b938a041363632a8b4f805" "d818d364712b551c535b952b3aa089c5941ef284" "7fe1e3de3e04afc43f9a3d3a8d38cd0a0efd9d4c" "d14db41612953d22506af16ef7a23c4d112150e5" "04fd52af504d80a42d9487e3e6aa96b6937255d1" "1f392dc4316da3e648c6dc0f4aad1a87d4be556c" "5600dc0bb4a2b72a613175da54edb4ad770105aa" "0174d99a8f1fdc506fa54403317072982656f127" default)))
 '(haskell-notify-p t)
 '(haskell-process-type (quote cabal-dev)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
