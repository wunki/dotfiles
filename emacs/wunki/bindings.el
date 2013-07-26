;; shortcuts
(global-set-key (kbd "<f9>") 'mu4e)
(global-set-key (kbd "<f10>") 'erc-start-or-switch)
(global-set-key (kbd "<f11>") 'jabber-start)

;; magit
(global-set-key (kbd "C-c C-g") 'magit-status)

;; search forward regexp
(global-set-key (kbd "C-c C-s") 'search-forward-regexp)

;; expand region (increases selected region by semantic units)
(global-set-key (kbd "C-c e") 'er/expand-region)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

;; edit file with sudo
(global-set-key (kbd "C-c s") 'sudo-edit)

;; join line command
(global-set-key (kbd "C-c j") 'join-line)

;; gotta have some fun
(global-set-key (kbd "C-c C-*") 'zone)

;; who needs shift when using forward/backwards paragraph
(global-set-key (kbd "M-[") 'backward-paragraph)
(global-set-key (kbd "M-]") 'forward-paragraph)

;; find recent files quickly
(global-set-key (kbd "C-c C-r") 'recentf-ido-find-file)

;; save/restore the current session
(global-set-key (kbd "<home>") 'my-desktop)

;; map execute command to the C-key
;; less hand movement with this command
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

;; kill word and avoid using the backspace
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; deleting and renaming of the active buffer
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; open a new line below or above
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; toggle between light and dark theme
(define-key global-map (kbd "C-c C-t") 'toggle-dark-light-theme)

;; github gists
(define-key global-map (kbd "C-S-c g") 'gist-buffer-private)
(define-key global-map (kbd "C-S-c b") 'gist-buffer)
(define-key global-map (kbd "C-S-c r") 'gist-region)

;; move buffers around
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; move current window into focus
(global-set-key (kbd "C-M-<return>")
                (lambda ()
                  (interactive)
                  (buf-move-left)
                  (buf-move-up)))

;; resize windows
(global-set-key (kbd "C-M-h")      'shrink-window-horizontally)
(global-set-key (kbd "C-M-l")      'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'enlarge-window)
(global-set-key (kbd "C-M-<up>")   'shrink-window)

;; move between windows
(global-set-key (kbd "C-c L") 'windmove-right)
(global-set-key (kbd "C-c H") 'windmove-left)
(global-set-key (kbd "C-c P") 'windmove-up)
(global-set-key (kbd "C-c N") 'windmove-down)

;; multiple markers
(key-chord-define-global "MM" 'mc/mark-next-like-this)
(key-chord-define-global "MA" 'mc/mark-all-like-this)
(key-chord-define-global "MD" 'mc/mark-all-like-this-dwim)

;; moving around
(key-chord-define-global "JJ" 'ace-jump-mode)
(key-chord-define-global "II" 'idomenu)

;; quickly edit files
(key-chord-define-global "EE" 'edit-config)
(key-chord-define-global "TT" 'edit-todos)

;; move around changes
(key-chord-define-global "CC" 'goto-last-change)

;; packages
(key-chord-define-global "UU" 'package-list-packages)

;; *tweet* *tweet*
(global-set-key (kbd "C-c t") 'twittering-update-status-interactive)

;; close emacs
(key-chord-define-global "ZZ" 'save-buffers-kill-emacs)
