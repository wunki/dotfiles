;; hide all the things
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; load theme and don't ask if it's save.
(load-theme 'sanityinc-tomorrow-blue t)

(set-frame-font "Dejavu Sans Mono")
(add-to-list 'default-frame-alist '(font . "Dejavu Sans Mono"))

(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

(provide 'wunki-theme)
