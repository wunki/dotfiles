;; disable scrollbars and menu bar on the mac. On Unix you can disable it in
;; Xdefaults.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; best place to start
(setq default-directory "~/Projects/")

(defun swap-meta-and-super ()
  "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (progn
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (message "Command is now bound to SUPER and Option is bound to META."))))

;; keybindings
(global-set-key (kbd "C-c w") 'swap-meta-and-super)
(global-set-key (kbd "M-`") 'ns-next-frame)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-H") 'ns-do-hide-others)

;; font ligatures
(mac-auto-operator-composition-mode t)

;; start out with meta as super, not the apple key
;; (swap-meta-and-super)

;; On Unix I start the server as a daemon at boot, on the Mac this needs to be
;; done here.
(server-start)

(provide 'wunki-mac)
