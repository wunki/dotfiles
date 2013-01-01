; load and fire
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'sendmail)

;; org-mode integration
(require 'org-mu4e)
(setq mu4e-org-contacts-file "~/org/contacts.org")
(add-to-list 'mu4e-headers-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)

;; Set mu4e as default mail agent
(setq mail-user-agent 'mu4e-user-agent)

;; Apply all the marks when leaving
(setq mu4e-headers-leave-behavior 'apply)

;; Headers
(setq mu4e-headers-fields
      '((:maildir       .   8) 
        (:date          .  24)
        (:flags         .   6)
        (:from          .  24)
        (:subject       .  nil)))

; set bookmarks
(setq mu4e-bookmarks 
  '(("flag:new maildir:/inbox"            "New Personal"         ?p)
    ("flag:new maildir:/bread-and-pepper" "New Bread & Pepper"   ?b)
    ("flag:unread AND NOT flag:trash"     "All unread messages"  ?u)
    ("date:today..now"                    "Today's messages"     ?t)
    ("date:7d..now"                       "Last 7 days"          ?w)
    ("mime:image/*"                       "Messages with images" ?i)
    ("flag:flagged"                       "Flagged messages"     ?f)))

(setq mu4e-user-mail-address-regexp
      "petar@wunki\.org\\|petar@breadandpepper\.com")

; maildir locations
(setq mu4e-maildir "/home/wunki/mail")
(setq mu4e-refile-folder "/archive")
(setq mu4e-sent-folder   "/sent")
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-trash-folder  "/trash")

; shortcuts
(setq mu4e-maildir-shortcuts
       '(("/inbox"             . ?i)
         ("/bread-and-pepper"  . ?b)
         ("/archive"           . ?a)
         ("/sent"              . ?s)
         ("/clojure"           . ?c)
         ("/haskell-beginners" . ?h)
         ("/trash"             . ?t)))

; sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 587)

; queue that mail
(setq smtpmail-queue-mail  nil  ;; start in non-queue mode
      smtpmail-queue-dir   "~/mail/queue/cur")

(setq mu4e-get-mail-command "offlineimap"
      mu4e-confirm-quit nil
      mu4e-headers-date-format "%d %b, %Y at %H:%M" ; date format
      message-signature "Petar Radosevic | @wunki"
      message-kill-buffer-on-exit t                 ; don' keep message buffers around
      mu4e-html2text-command "html2text -utf8 -width 72"
)
