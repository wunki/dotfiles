;; mac only settings.
;; use the old way of toggling to fullscreen
(set-frame-font "Fira Mono-13")
(add-to-list 'default-frame-alist '(font . "Fira Mono 13"))

;; browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; set the correct path on the Mac
(when (file-name-nondirectory (getenv "SHELL")) "fish"
   (setq path-separator " ")
   (exec-path-from-shell-initialize)
   (setq path-separator ":"))

(provide 'wunki-mac)
