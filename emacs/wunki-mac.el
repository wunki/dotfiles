;; mac only settings.
;; use the old way of toggling to fullscreen
(set-frame-font "Ubuntu Mono-16")
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 16"))

;; browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(provide 'wunki-mac)
