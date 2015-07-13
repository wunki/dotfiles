;; show column numbers
(column-number-mode t)

;; don't show the menubar
(menu-bar-mode -1)

;; highlight the current line
(global-hl-line-mode -1)

;; projectile for project management
(projectile-global-mode)

;; deletes region when starting typing
(pending-delete-mode t)

;; magit
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-git-executable "/usr/local/bin/git")

;; blinking
(blink-cursor-mode -1)

;; auto-completion
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(add-hook 'after-init-hook 'global-company-mode)     ; auto-complete in every buffer

;; improve buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; auto revert changes on disk
(global-auto-revert-mode t)

;; powerline
(powerline-default-theme)

;; indent after newline
(electric-indent-mode t)
(add-hook 'yaml-mode-hook 'disable-electric-indent)
(add-hook 'org-mode-hook 'disable-electric-indent)
(add-hook 'haskell-mode-hook 'disable-electric-indent)

;; recently opened files
(recentf-mode 1)
(setq recentf-max-saved-items 30)
(add-to-list 'recentf-exclude "\\/tmp$")
(add-to-list 'recentf-exclude "\\.last$")
(add-to-list 'recentf-exclude "elpa")

;; javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)

;; jinja
(add-to-list 'auto-mode-alist '("\\.j2$" . jinja2-mode))

;; whitespace
(setq whitespace-style
      '(face tabs spaces trailing lines space-before-tab
             newline indentation space-after-tab tab-mark newline-mark)
      whitespace-display-mappings
      '((space-mark   ?\    [?\xB7]     [?.])     ; space
        (space-mark   ?\xA0 [?\xA4]     [?_])     ; hard space
        (newline-mark ?\n   [?\xAB ?\n] [?$ ?\n]) ; end-of-line
        ))

;; ag (the silver searcher)
(require 'ag)
(setq ag-highlight-search t)

;; dired
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; ido-mode
(ido-mode t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-use-faces nil                ; don't use faces because we have flx
      ido-use-filename-at-point nil
      ido-max-prospects 10
      ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
        "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
        "_region_" " output\\*$" "^TAGS$" "^\*Ido")
      ido-ignore-directories
      '("\\`auto/" "\\`auto-save-list/" "\\`backups/" "\\`semanticdb/"
        "\\`target/" "\\`\\.git/" "\\`\\.svn/" "\\`CVS/" "\\`\\.\\./"
        "\\`.sass-cache/" "\\`\\./" "\\`Godeps/")
      ido-ignore-files
      '("\\`auto/" "\\.prv/" "_region_" "\\.class/"  "\\`CVS/" "\\`#"
        "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.hi" "\\.org_archive" "\\.test"))

; auto-fill
(add-hook 'html-mode-hook 'turn-off-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'clojure-mode-hook 'my-auto-fill-prog)

;; flycheck
(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; flyspell
(add-hook 'clojure-mode-hook 'flyspell-prog-mode)
(add-hook 'haskell-mode-hook 'flyspell-prog-mode)
(add-hook 'python-mode-hook 'flyspell-prog-mode)
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(setq flyspell-issue-message-flag nil)             ; don't show a message, slows things down.

;; PO
(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; markdown
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.page" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook (lambda ()
                                (orgtbl-mode t)))

;; puppet
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; salt stack
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

;; cfengine3
(add-to-list 'auto-mode-alist '("\\.cf$" . cfengine3-mode))

;; edit rest documentation
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))

;; window movement
(setq windmove-wrap-around t)

;; nice visualation of undo's
(global-undo-tree-mode t)

;; twittering
(setq twittering-use-master-password t)
(setq twittering-curl-program "/usr/bin/curl")
(add-hook 'twittering-edit-mode-hook
          (lambda ()
            (auto-fill-mode -1)))
(if (eq system-type 'berkeley-unix)
    (setq twittering-curl-program "/usr/local/bin/curl")) 

;; jump to the last place you were in the file
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saved-places")
(setq-default save-place t)

;; smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit
      sp-autoskip-closing-pair 'always
      sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(smartparens-global-mode t)

;; display search jumps
(global-anzu-mode t)

;; cleanup modeline
(diminish 'projectile-mode)
(diminish 'undo-tree-mode)
(diminish 'anzu-mode)
(after-load 'company-mode
  (diminish 'company-mode))
(diminish 'smartparens-mode)
(after-load 'subword
  (diminish 'subword-mode))

(provide 'wunki-modes)
