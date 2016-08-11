;; hide all the things
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; load theme and don't ask if it's save.
(load-theme 'sanityinc-tomorrow-blue t)

;; font face
(let ((font "Fira Mono")
      (size (if (mac?) 15 9)))
  (add-to-list 'default-frame-alist (cons 'font (format "%s %s" font size)))
  (set-frame-font (format "%s %s" font size) nil t))

;; font size
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

(provide 'wunki-theme)
