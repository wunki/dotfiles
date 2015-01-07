;; mac only settings.
(set-frame-font "Fira Mono-16")
(add-to-list 'default-frame-alist '(font . "Fira Mono 16"))

;; disable scrollbars and menu bar on the mac. On Linux you can disable it in
;; Xdefaults.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; correct path
(exec-path-from-shell-initialize)

;; copy some vars over
(exec-path-from-shell-copy-env "GOPATH")
(exec-path-from-shell-copy-env "PYTHONPATH")

;; emacsclient as editor for Magit
(setenv "EDITOR" "emacsclient")

;; dash for documentation
(global-set-key (kbd "C-c d") 'dash-at-point)
(global-set-key (kbd "C-c C-d") 'dash-at-point-with-docset)

;; Set the Apple key as Meta
;; (setq mac-command-modifier 'meta)
;; (setq mac-option-modifier 'none)
(setq default-input-method "MacOSX")
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-H") 'ns-do-hide-others)

;; On Unix I start the server as a daemon at boot, on the Mac this needs to be
;; done here.
(server-start)

(provide 'wunki-mac)
