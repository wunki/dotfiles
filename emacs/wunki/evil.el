;; evil mode (vim emulation)
(evil-mode t)
(global-surround-mode t)

(mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
       '(inferior-emacs-lisp-mode
         comint-mode
         shell-mode
         term-mode
         jabber-roster-mode
         jabber-chat-mode
         magit-branch-manager-mode))
