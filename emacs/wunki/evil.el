;; evil mode (vim emulation)
;; going to try this out one day
(evil-mode -1)
(global-surround-mode -1)

(mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
       '(inferior-emacs-lisp-mode
         comint-mode
         shell-mode
         term-mode
         jabber-roster-mode
         jabber-chat-mode
         magit-branch-manager-mode))

;; Mu4e
(eval-after-load 'mu4e
  '(progn
     ;; use the standard bindings as a base
     (evil-make-overriding-map mu4e-view-mode-map 'normal t)
     (evil-make-overriding-map mu4e-main-mode-map 'normal t)
     (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
     
     (evil-add-hjkl-bindings mu4e-view-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "C" 'mu4e-compose-new
       "o" 'mu4e-view-message
       "Q" 'mu4e-raw-view-quit-buffer)
     
     ;; (evil-add-hjkl-bindings mu4e-view-raw-mode-map 'normal
     ;; "J" 'mu4e-jump-to-maildir
     ;; "j" 'evil-next-line
     ;; "C" 'mu4e-compose-new
     ;; "q" 'mu4e-raw-view-quit-buffer)
     
     (evil-add-hjkl-bindings mu4e-headers-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "C" 'mu4e-compose-new
       "o" 'mu4e-view-message
       )
     
     (evil-add-hjkl-bindings mu4e-main-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'evil-next-line
       "RET" 'mu4e-view-message)))
