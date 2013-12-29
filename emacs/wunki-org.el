;; org-mode
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c b") 'org-iswitchb)
(define-key global-map (kbd "<f8>") 'org-cycle-agenda-files)

;; load org-protocol for external calls
(require 'org-protocol)

;; use contact manager from org
(require 'org-contacts)
(setq org-contacts-files (list "~/org/contacts.org"))

;; archive method
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;; org-files
(setq org-agenda-files (list "~/org/today.org"
                             "~/org/inbox.org"
                             "~/org/gibbon.org"
                             "~/org/bread-and-pepper.org"
                             "~/org/personal.org"
                             "~/org/emacs.org"
                             "~/org/notes.org"
                             "~/org/ideas.org"
                             "~/org/books.org"
                             "~/org/courses.org"
                             "~/org/contacts.org"

                             ; projects
                             "~/python/gibbon-web/TODO.org"
                             "~/src/wunki-dotfiles/TODO.org"
                             "~/devops/wunki-park/TODO.org")
      org-default-notes-file "~/org/inbox.org")

;; always use indent-mode
(setq org-startup-indented nil)

;; switch between tasks states
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

;; log the time
(setq org-log-done 'time)

;; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

;; refile items two levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

;; targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

;; allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (sh . t)
   (ditaa . t)
   (python . t)
   (haskell . t)
   (scala . t)
   (scheme . t)
   (clojure . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

;; capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")
        ("n" "Note" entry (file "~/org/notes.org")
         "* NOTE %?\n  %i\n  %a")
        ("w" "" entry (file+headline "~/org/websites.org" "Notes")
         "* %^{Title}\n\n  Source: %u, %c\n\n  %i")))

;; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?E)
                            ("@work" . ?W)
                            ("@home" . ?H)
                            (:endgroup)
                            ("mail" . ?m)
                            ("write" . ?w)
                            ("phone" . ?c)
                            ("waiting" . ?.)
                            ("hold" . ?h)
                            ("code" . ?c)
                            ("note" . ?n)
                            ("cancelled" . ?a)
                            ("flagged" . ??))))

;; automagically update the counter
(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

(provide 'wunki-org)
