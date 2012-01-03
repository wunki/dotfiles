;; hack to open .m and .h files in objc-mode
(setq auto-mode-alist (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h$" . objc-mode) auto-mode-alist))
(add-hook 'objc-mode-hook '(lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;; let's ff-find-other-file also find objective-c header files
(add-hook 'objc-mode-hook
          (lambda ()
            (set (make-local-variable 'cc-other-file-alist)  '(("\\.m\\'" (".h")) ("\\.h\\'" (".m" ".c" ".cpp"))))))

; ido mode
(ido-mode 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ; ido-create-new-buffer 'always
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
        "\\`.#" "\\`\\.\\./" "\\`\\./" "\\.hi$"))

; html
(add-hook 'html-mode-hook 'turn-off-auto-fill)

; haskell mode
(load "~/.emacs.d/elpa/haskell-mode-2.8.0/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

; po-mode
(setq auto-mode-alist 
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

; deft
(setq deft-directory "~/Dropbox/Documents/Notes/")
(setq deft-text-mode 'markdown-mode)

; paredit
(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

; markdown mode
(setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook 'turn-on-pandoc)

; org-mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(setq org-agenda-files (list "~/Documents/Org/personal.org"
                             "~/Documents/Org/bread-and-pepper.org"
                             "~/Documents/Org/books.org"
                             "~/Documents/Org/inbox.org"))

(setq org-directory (expand-file-name "~/Documents/Org"))
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
(add-to-list 'auto-mode-alist '(".*mutt.*" . mail-mode))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook
 'mail-mode-hook
 (lambda ()
   (define-key mail-mode-map [(control c) (control c)]
     (lambda ()
       (interactive)
       (save-buffer)
       (server-edit)))))
