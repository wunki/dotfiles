;; mac only settings.
;; use the old way of toggling to fullscreen
(set-frame-font "Fira Mono-14")
(add-to-list 'default-frame-alist '(font . "Fira Mono 14"))

;; browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; correct path
(exec-path-from-shell-initialize)

;; language specific settings
(exec-path-from-shell-copy-env "GOPATH")

;; Set the Apple key as Meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)
(setq default-input-method "MacOSX")
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-H") 'ns-do-hide-others)

(provide 'wunki-mac)
