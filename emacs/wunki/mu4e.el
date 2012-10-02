; load and fire
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'sendmail)

;; org-mode integration
(require 'org-mu4e)
(setq mu4e-org-contacts-file (concat dropbox-directory "/Org/contacts.org"))
(add-to-list 'mu4e-headers-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)
(add-to-list 'mu4e-view-actions
             '("org-contact-add" . mu4e-action-add-org-contact) t)

; set default mail agent
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-user-mail-address-regexp
      "petar@wunki\.org\\|petar@breadandpepper\.com")

; maildir locations
(setq mu4e-maildir "/home/wunki/mail")
(setq mu4e-refile-folder "/archive")
(setq mu4e-sent-folder   "/sent")
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-trash-folder  "/trash")

; shortcuts
(setq  mu4e-maildir-shortcuts
       '(("/inbox"            . ?i)
         ("/bread-and-pepper" . ?b)
         ("/archive"          . ?a)
         ("/sent"             . ?s)
         ("/trash"            . ?t)))

; sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 587)

; queue that mail
(setq smtpmail-queue-mail  nil  ;; start in non-queue mode
      smtpmail-queue-dir   "~/mail/queue/cur")

(setq mu4e-get-mail-command "offlineimap -a wunki"
      mu4e-confirm-quit nil
      mu4e-headers-date-format "%d %b, %Y at %H:%M" ;; date format
      message-signature "Petar Radosevic | @wunki"
      message-kill-buffer-on-exit t)                ;; don' keep message buffers around
