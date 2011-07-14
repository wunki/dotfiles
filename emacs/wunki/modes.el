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
                             "~/Documents/Notes/personal.org"))
(setq org-directory (expand-file-name "~/Documents/Notes"))
(setq org-log-done 'time)
(setq org-default-notes-file (concat org-directory "/inbox.org"))

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
