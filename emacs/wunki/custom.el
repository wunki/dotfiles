;; default tab-width is two spaces
(setq-default tab-width 2
              js-indent-level 2
              c-basic-offset 2
              indent-tabs-mode nil)

;; email
(setq user-full-name "Petar Radosevic")
(setq user-mail-address "petar@wunki.org")

;; conkeror as browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/home/wunki/bin/conk")

;; unicode
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; warnings
(setq visible-bell t)
(setq ring-bell-function (lambda nil (message "")))

;; scrolling
(setq scroll-conservatively 10000) ; don't jump my screen!

;; autosave
(setq auto-save-interval 500)

;; follow symlinks and don't ask questions
(setq vc-follow-symlinks t) 

;; always show the region
(setq transient-mark-mode t)

;; enable clipboard on x
(setq x-select-enable-clipboard t)

;; format the title-bar to always include the buffer name
(setq frame-title-format " %b (%m)")

;; autoselect window with mouse
(setq mouse-autoselect-window t)

;; dont show the GNU splash screen
(setq inhibit-startup-message t)

;; dont ask for yes or no, just use y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; auto-fill
(setq-default fill-column 78)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; truncate long lines
(setq-default truncate-lines nil)

;; european dates
(setq calendar-date-style 'european)

;; spelling
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")

;; call me crazy but I only want lowercase on my linux system
(setq dropbox-directory (if (string-equal system-type "gnu/linux")
                            (expand-file-name "~/dropbox")
                          (expand-file-name "~/Dropbox")))

;; disable scrollbars and menu bar on the mac
;; disable it with .Xdefaults in linux
(when (string-equal system-type "darwin")
  (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
  (when (fboundp 'menu-bar-mode) (menu-bar-mode -1)))

;; theme security by emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "405c5240a912732cac2b373354729594c56d6edee0962d5a848429a4e4e3edef" "ad63a0dfb8bd5d4c0c7015e851a09283ff050b8df37d2ffe2028be77ff446119" "84c93dd294de8d877259fe2a4ab6540aaadbba3fbeb466692187f7a265c41203" "f5e56ac232ff858afb08294fc3a519652ce8a165272e3c65165c42d6fe0262a0" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4d66773cc6d32566eaf2c9c7ce11269d9eb26e428a1a4fa10e97bae46ff615da" "d05303816026cec734e26b59e72bb9e46480205e15a8a011c62536a537c29a1a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "be7eadb2971d1057396c20e2eebaa08ec4bfd1efe9382c12917c6fe24352b7c1" default)))
 '(haskell-notify-p t)
 '(haskell-process-type (quote ghci))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(jabber-events-request-these (quote (delivered displayed composing)))
 '(safe-local-variable-values (quote ((pony-settings make-pony-project :python "/home/wunki/.virtualenvs/gibbon-web/bin/python") (pony-settings make-pony-project :python "/home/wunki/.virtualenvs/mijnmazda/bin/python") (pony-settings make-pony-project :python "/home/wunki/.virtualenvs/gibbon/bin/python")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-prompt-face ((t (:foreground "#81a2be" :weight bold))))
 '(italic ((t (:slant italic))))
 '(jabber-chat-prompt-foreign ((t (:foreground "#dfaf8f"))))
 '(jabber-chat-prompt-local ((t (:foreground "#f0dfaf"))))
 '(jabber-chat-prompt-system ((t (:foreground "#8fb28f" :weight bold))))
 '(jabber-roster-user-away ((t (:foreground "#bc8383"))))
 '(jabber-roster-user-chatty ((t (:foreground "#9fc59f"))))
 '(jabber-roster-user-online ((t (:foreground "#bfebbf")))))
