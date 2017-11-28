;; load org-protocol for external calls
(require 'org-protocol)

;; org-mode
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c b") 'org-iswitchb)
(define-key global-map (kbd "<f8>") 'org-cycle-agenda-files)

;; root directory of org files
(setq org-root (expand-file-name "~/Org"))

;; journal
(setq org-journal-dir (format "%s/%s" org-root "journal"))

;; archive method
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;; org-files, only used for coding
(setq org-agenda-files (list (format "%s/%s" org-root "inbox.org")
                             (format "%s/%s" org-root "today.org")
                             (format "%s/%s" org-root "personal.org")
                             (format "%s/%s" org-root "degreed.org")
                             (format "%s/%s" org-root "books.org")
                             (format "%s/%s" org-root "courses.org"))
      org-default-notes-file (format "%s/%s" org-root "inbox.org"))

;; always use indent-mode
(setq org-startup-indented t)

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

;; enable exporting as markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (ditaa . t)
   (python . t)
   (haskell . t)
   (scheme . t)
   (clojure . t)))

(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

;; capture templates
(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-root "/inbox.org") "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+olp+datetree ,(concat org-root "/journal.org"))
             "* %?\nEntered on %U\n  %i\n  %a")))

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
