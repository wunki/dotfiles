; hack to open .m and .h files in objc-mode
(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h$" . objc-mode) auto-mode-alist))
(add-hook 'objc-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

; markdown mode
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

; org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(setq org-agenda-files (list "~/Documents/Notes/bread-and-pepper.org"
                             "~/Documents/Notes/personal.org" 
                             "~/Documents/Notes/books.org"
                             "~/Documents/Notes/inbox.org"))

(setq org-directory (expand-file-name "~/Documents/Notes"))
(setq org-log-done 'time)
(setq org-default-notes-file (concat org-directory "/inbox.org"))

; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

; refile items two levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

; targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)

; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (sh . t)
   (ruby . t)
   (python . t)
   (js . t)
   (haskell . t)
   (clojure . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

; puppet mode
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

; vagrant
(add-to-list 'auto-mode-alist '("VagrantFile" . ruby-mode))

; mutt
(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))                                                                   
  (setq mail-header-separator "")                                                                                               
  (add-hook 'message-mode-hook
          'turn-on-auto-fill
          (lambda ()
             (progn
               (local-unset-key (kbd "C-c C-c"))
               (define-key message-mode-map (kbd "C-c C-c") '(lambda ()
                                                               "save and exit quickly"
                                                               (interactive)
                                                               (save-buffer)
                                                               (server-edit))))))
