;; load and fire
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'sendmail)
(require 'org-mu4e)

;; set mu4e as default mail agent
(setq mail-user-agent 'mu4e-user-agent)

;; default maildir locations
(setq mu4e-maildir "/home/wunki/mail"
      mu4e-sent-folder "/wunki/sent"
      mu4e-drafts-folder "/wunki/drafts"
      mu4e-trash-folder "/wunki/trash")

;; multiple accounts
(setq wunki-mu4e-account-alist
      '(("wunki"
         (user-mail-address "petar@wunki.org")
         (mu4e-sent-folder "/wunki/sent")
         (mu4e-drafts-folder "/wunki/drafts")
         (mu4e-refile-folder "/wunki/archive")
         (mu4e-trash-folder  "/wunki/trash")
         (smtpmail-smtp-server "mail.messagingengine.com"))
        ("bread-and-pepper"
         (user-mail-address "petar@breadandpepper.com")
         (mu4e-sent-folder "/bread-and-pepper/sent")
         (mu4e-drafts-folder "/bread-and-pepper/drafts")
         (mu4e-refile-folder "/bread-and-pepper/archive")
         (mu4e-trash-folder  "/bread-and-pepper/trash")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user "petar@breadandpepper.com"))
        ("gibbon"
         (user-mail-address "petar@gibbon.co")
         (mu4e-sent-folder "/bread-and-pepper/sent")
         (mu4e-drafts-folder "/bread-and-pepper/drafts")
         (mu4e-refile-folder "/bread-and-pepper/archive")
         (mu4e-trash-folder  "/bread-and-pepper/trash")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user "petar@breadandpepper.com"))))

(defun wunki-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-msg-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var)) wunki-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) wunki-mu4e-account-alist)
                             nil t nil nil (caar wunki-mu4e-account-alist))))
         (account-vars (cdr (assoc account wunki-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars))))
(add-hook 'mu4e-compose-pre-hook 'wunki-mu4e-set-account)

;; org-mode integration
(setq mu4e-org-contacts-file "/home/wunki/org/contacts.org")
(add-to-list 'mu4e-headers-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)

;; apply all the marks when leaving
(setq mu4e-headers-leave-behavior 'apply)

;; headers
(setq mu4e-headers-fields
      '((:maildir       .   8) 
        (:date          .  24)
        (:flags         .   6)
        (:from          .  24)
        (:subject       .  nil)))

;; set bookmarks
(setq mu4e-bookmarks 
  '(("flag:new maildir:/wunki/inbox"              "New Personal"         ?p)
    ("flag:new maildir:/bread-and-pepper/inbox"   "New Bread & Pepper"   ?b)
    ("flag:unread maildir:/wunki/inbox OR maildir:/bread-and-pepper/inbox"  "All unread messages"  ?u)
    ("date:today..now"                            "Today's messages"     ?t)
    ("date:7d..now"                               "Last 7 days"          ?w)
    ("mime:image/*"                               "Messages with images" ?i)
    ("flag:flagged"                               "Flagged messages"     ?f)))

(setq mu4e-user-mail-address-regexp
      "petar@wunki\.org\\|petar@breadandpepper\.com")

;; shortcuts
(setq mu4e-maildir-shortcuts
       '(("/wunki/inbox"              . ?i)
         ("/bread-and-pepper/inbox"   . ?I)
         ("/wunki/archive"            . ?a)
         ("/bread-and-pepper/archive" . ?A)
         ("/wunki/sent"               . ?s)
         ("/bread-and-pepper/sent"    . ?S)
         ("/wunki/trash"              . ?t)
         ("/bread-and-pepper/trash"   . ?T)
         ("/wunki/clojure"            . ?c)
         ("/wunki/haskell-beginners"  . ?h)
         ("/wunki/rust-development"   . ?r)))

;; sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

;; queue that mail
(setq smtpmail-queue-mail  nil  ;; start in non-queue mode
      smtpmail-queue-dir   "~/mail/queue/cur")

(setq mu4e-get-mail-command "offlineimap"
      mu4e-confirm-quit nil
      mu4e-headers-date-format "%d %b, %Y at %H:%M" ; date format
      message-signature "Petar Radosevic | @wunki"  ; signature
      message-kill-buffer-on-exit t                 ; don' keep message buffers around
      mu4e-html2text-command "html2text -utf8 -width 72")
