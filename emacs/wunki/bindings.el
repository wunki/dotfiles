; shortcuts
(global-set-key "\C-xg" 'magit-status)

; map execute command to the C-key
; less hand movement with this command
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; kill word and avoid using the backspace
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-k" 'kill-region)

; temporary disable arrow keys so I'm forced to learn emacs movements
(global-unset-key [(up)])
(global-unset-key [(down)])
(global-unset-key [(left)])
(global-unset-key [(right)])

; set the command key as meta
; (setq mac-command-modifier 'meta)

; no mailing, thanks
(global-unset-key (kbd "C-x m"))
(global-unset-key "\C-z")
