;; shortcuts
(global-set-key (kbd "<f9>") 'mu4e)
(global-set-key (kbd "<f10>") 'erc-start-or-switch)
(global-set-key (kbd "<f11>") 'cfw:open-calendar-buffer)

;; search forward regexp
(global-set-key (kbd "C-c C-s") 'search-forward-regexp)

;; expand region (increases selected region by semantic units)
(global-set-key (kbd "C-c e") 'er/expand-region)
(global-set-key (kbd "M-i") 'change-inner)


;; edit file with sudo
(global-set-key (kbd "C-c s") 'sudo-edit)

;; join line command
(global-set-key (kbd "C-c C-j") 'join-line)

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

;; github gists
(global-set-key (kbd "C-S-c g") 'gist-buffer-private)
(global-set-key (kbd "C-S-c b") 'gist-buffer)
(global-set-key (kbd "C-S-c r") 'gist-region)

;; move buffers around
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

;; move lines
(global-set-key [(control shift up)] 'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)

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
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c F") 'windmove-right)
(global-set-key (kbd "C-c B") 'windmove-left)
(global-set-key (kbd "C-c P") 'windmove-up)
(global-set-key (kbd "C-c N") 'windmove-down)

;; multiple markers
(global-set-key (kbd "C-!") 'mc/edit-lines)
(global-set-key (kbd "C-@") 'mc/mark-next-like-this)
(global-set-key (kbd "C-#") 'mc/mark-all-like-this-dwim)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; moving around
(global-set-key (kbd "M-1") 'ace-jump-mode)
(global-set-key (kbd "M-2") 'ace-window)
(global-set-key (kbd "C-c C-i") 'idomenu)

;; quickly edit files
(global-set-key (kbd "C-c 1") 'ido-emacs-config)
(global-set-key (kbd "C-c 2") 'projectile-switch-project)
(global-set-key (kbd "C-c 3") 'ido-todos)

;; unfill a paragraph
(global-set-key (kbd "C-c u") 'wunki/unfill-paragraph)

;; move around changes
(global-set-key (kbd "C-c C-l") 'goto-last-change)

;; undo tree
(global-set-key (kbd "C-c -") 'undo-tree-visualize)

;; packages
(global-set-key (kbd "C-c C-u") 'package-list-packages)

;; whitespace mode
(global-set-key (kbd "C-c C-w") 'global-whitespace-mode)

;; close emacs
(global-set-key (kbd "M-RET") 'ansi-term)

;; close emacs
(global-set-key (kbd "C-c q") 'save-buffers-kill-emacs)

;; fullscreen
(global-set-key (kbd "<f12>") 'toggle-frame-fullscreen)

;; newline and indent in yaml
(eval-after-load "yaml-mode"
  '(progn
     (define-key yaml-mode-map (kbd "RET") 'newline-and-indent)))

(provide 'wunki-bindings)
