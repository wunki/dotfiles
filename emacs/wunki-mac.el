;; mac only settings.
;; use the old way of toggling to fullscreen
(set-frame-font "Menlo-14")
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 16"))

;; browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; set the correct path on the Mac
(exec-path-from-shell-initialize)

(provide 'wunki-mac)
