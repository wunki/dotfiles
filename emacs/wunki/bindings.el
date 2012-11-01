;;shortcuts
(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "<f9>") 'mu4e)
(global-set-key (kbd "<f10>") 'jabber-start)

;;training wheels
(global-unset-key (kbd "C-x o"))

;;search forward regexp
(global-set-key (kbd "C-c C-s") 'search-forward-regexp)

;;fullscreen on mac
(if (eq system-type 'darwin)
    (global-set-key (kbd "<f3>") 'ns-toggle-fullscreen))

; who needs shift when using forward/backwards paragraph
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;;map execute command to the C-key
;;less hand movement with this command
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;;kill word and avoid using the backspace
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-k") 'kill-region)

;;ace-jump and idomenu for moving around
(define-key global-map (kbd "C-&") 'ace-jump-mode)
(define-key global-map (kbd "C-*") 'idomenu)

;;helm
(global-set-key (kbd "C-c h") 'helm-projectile)

;;don't suspend emacs
(global-unset-key (kbd "C-z"))

;;gist.github.com
(define-key global-map (kbd "C-c C-c C-g") 'gist-buffer)
(define-key global-map (kbd "C-c C-c C-p") 'gist-region)

;;quickly edit emacs config with ido
(define-key global-map (kbd "C-c C-x C-e") 
  '(lambda () 
     (interactive)
     (ido-find-file-in-dir "~/.emacs.d/wunki")))

;;move buffers around
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;move between windows
(global-set-key (kbd "M-N") 'select-next-window)
(global-set-key (kbd "M-P") 'select-previous-window)

;;focus on this window
(global-set-key (kbd "C-M-<return>") (lambda ()
                                       (interactive)
                                       (buf-move-left)
                                       (buf-move-up)))

;;resize windows
(global-set-key (kbd "C-M-l") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-h") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)

;;quickly edit org files with ido
(define-key global-map (kbd "C-c C-x C-t") 
  '(lambda () 
     (interactive)
     (ido-find-file-in-dir (concat dropbox-directory "/Org"))))
