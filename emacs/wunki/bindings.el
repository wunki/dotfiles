; shortcuts
(global-set-key (kbd "C-c C-g") 'magit-status)

; fullscreen on mac
(if (eq system-type 'darwin)
    (global-set-key (kbd "<f3>") 'ns-toggle-fullscreen))

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-g") 'goto-line)
(add-hook 'objc-mode-hook '(lambda ()
                            (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(global-set-key (kbd "<f9>") 'global-linum-mode)

; who needs shift when using forward/backwards paragraph
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

; map execute command to the C-key
; less hand movement with this command
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

; kill word and avoid using the backspace
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-c C-k") 'kill-region)

; ace-jump and idomenu for moving around
(define-key global-map (kbd "C-&") 'ace-jump-mode)
(define-key global-map (kbd "C-*") 'idomenu)

; don't suspend emacs
(global-unset-key (kbd "C-z"))

; gist.github.com
(define-key global-map (kbd "C-c p") 'gist-buffer)
(define-key global-map (kbd "C-c C-p") 'gist-region)

; quickly edit emacs config with ido
(define-key global-map (kbd "C-c C-x C-e") 
  '(lambda () 
     (interactive)
     (ido-find-file-in-dir "~/.emacs.d/wunki")))

; quickly edit org files with ido
(define-key global-map (kbd "C-c C-x C-t") 
  '(lambda () 
     (interactive)
     (ido-find-file-in-dir (concat dropbox-directory "/Org"))))
