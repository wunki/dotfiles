;; mac only settings.
;; use the old way of toggling to fullscreen
(set-frame-font "Fira Mono-13")
(add-to-list 'default-frame-alist '(font . "Fira Mono 13"))

;; browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Correct path
(exec-path-from-shell-initialize)

;; Set the Apple key as Meta
;; (setq ns-command-modifier 'meta)

(provide 'wunki-mac)
