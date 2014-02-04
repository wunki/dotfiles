;; show column numbers
(column-number-mode t)

;; don't show the menubar
(menu-bar-mode -1)

;; highlight the current line
(global-hl-line-mode t)

;; projectile for project management
(projectile-global-mode)

;; deletes region when starting typing
(pending-delete-mode t)

;; blinking
(blink-cursor-mode t)

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

;; calfw, handy calendar
(require 'calfw)

;; recently opened files
(recentf-mode 1)
(setq recentf-max-saved-items 30)
(add-to-list 'recentf-exclude "\\/tmp$")
(add-to-list 'recentf-exclude "\\.last$")
(add-to-list 'recentf-exclude "elpa")

;; javascript and json
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

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

;; auto completion
(require 'auto-complete-config)
(ac-config-default)
(setq ac-auto-start nil)    ; don't automatically trigger auto-complete
(ac-set-trigger-key "TAB")  ; only trigger auto-completion on TAB

;; ag (the silver searcher)
(require 'ag)
(setq ag-highlight-search t)

;; websites
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist '(("django" . "\\.html\\'")))

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
        "\\`\\./")
      ido-ignore-files
      '("\\`auto/" "\\.prv/" "_region_" "\\.class/"  "\\`CVS/" "\\`#"
        "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.hi" "\\.org_archive"))

; auto-fill
(add-hook 'html-mode-hook 'turn-off-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'clojure-mode-hook 'my-auto-fill-prog)

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
(add-hook 'markdown-mode-hook 'turn-on-pandoc)

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

;; don't mess with these pairs in Django templates
(sp-local-pair 'web-mode "{%" "%}")
(sp-local-pair 'web-mode "{{" "}}")
(sp-local-pair 'web-mode "{" nil :actions nil)

;; display search jumps
(global-anzu-mode t)

;; cleanup modeline
(diminish 'projectile-mode)
(diminish 'auto-complete-mode)
(diminish 'undo-tree-mode)
(diminish 'anzu-mode)

(provide 'wunki-modes)
