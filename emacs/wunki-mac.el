;; mac only settings.
;; use the old way of toggling to fullscreen
(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "<f3>") 'toggle-frame-fullscreen)
(set-face-attribute 'default nil :height 160)

;; browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; extend exec-path for homebrew
(setq exec-path (append exec-path '("/usr/local/bin")))

(provide 'wunki-mac)
