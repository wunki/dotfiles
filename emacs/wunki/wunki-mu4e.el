;; load and fire
(add-to-list 'load-path "/usr/share/emacs24/site-lisp/mu4e")

(require 'mu4e)
(require 'sendmail)
(require 'org-mu4e)

;; hooks
(add-hook 'mu4e-compose-mode-hook
          (defun my-compose-stuff ()
            (set-fill-column 72)
            (flyspell-mode)))

;; my e-mail addresses
(setq mu4e-user-mail-address-list '("petar@wunki.org"
                                    "petar@gibbon.co"
                                    "petar@breadandpepper.com"
                                    "hello@breadandpepper.com"
                                    "hello@invyapp.com"
                                    "hello@gibbon.co"))

;; reply attribution line
(setq message-citation-line-format "On %a, %b %d %Y, %N wrote:")
(setq message-citation-line-function 'message-insert-formatted-citation-line)

;; general settings
(setq mail-user-agent 'mu4e-user-agent                   ; mu4e as default mail agent
      mu4e-attachment-dir "~/downloads"                  ; put attachements in download dir
      mu4e-get-mail-command "offlineimap"                ; fetch email with offlineimap
      mu4e-confirm-quit nil                              ; don't ask me to quit
      mu4e-headers-skip-duplicates t                     ; skip duplicate email, great for gmail
      mu4e-headers-date-format "%d %b, %Y at %H:%M"      ; date format
      mu4e-headers-leave-behavior 'apply                 ; apply all marks at quit
      mu4e-html2text-command "w3m -dump -T text/html"    ; html to text
      mu4e-compose-dont-reply-to-self t                  ; don't reply to myself
      mu4e-compose-complete-only-personal t              ; only personal messages get in the address book
      mu4e-use-fancy-chars t                             ; use fancy characters
      mu4e-compose-signature "Petar Radosevic | @wunki"  ; signature
      message-kill-buffer-on-exit t                      ; don't keep message buffers around
      smtpmail-queue-mail nil                            ; start in non queue mode
)

;; maildir locations
(setq mu4e-maildir "/home/wunki/mail"
      mu4e-sent-folder "/wunki/sent"
      mu4e-drafts-folder "/wunki/drafts"
      mu4e-trash-folder "/wunki/trash"
      mu4e-refile-folder "/wunki/archive"
      smtpmail-queue-dir   "~/mail/queue/cur")

;; sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

;; set the archive according to the mailbox
(setq mu4e-refile-folder
      (lambda (msg)
        (if msg
            (let ((account (nth 1 (split-string (mu4e-message-field msg :maildir) "/"))))
              (format "/%s/archive" account)))))

;; set the trash according to the mailbox
(setq mu4e-trash-folder
      (lambda (msg)
        (if msg
            (let ((account (nth 1 (split-string (mu4e-message-field msg :maildir) "/"))))
              (format "/%s/trash" account)))))

;; multiple accounts
(setq wunki-mu4e-account-alist
      '(("gibbon"
         (user-mail-address "petar@gibbon.co")
         (mu4e-sent-folder "/gibbon/sent")
         (mu4e-drafts-folder "/gibbon/drafts")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user "petar@gibbon.co"))
        ("wunki"
         (user-mail-address "petar@wunki.org")
         (mu4e-sent-folder "/wunki/sent")
         (mu4e-drafts-folder "/wunki/drafts")
         (smtpmail-smtp-server "mail.messagingengine.com")
         (smtpmail-smtp-user "wunki@fastmail.fm"))
        ("bread-and-pepper"
         (user-mail-address "petar@breadandpepper.com")
         (mu4e-sent-folder "/bread-and-pepper/sent")
         (mu4e-drafts-folder "/bread-and-pepper/drafts")
         (smtpmail-smtp-server "smtp.gmail.com")
         (smtpmail-smtp-user "petar@breadandpepper.com"))))

(defun wunki-mu4e-set-account ()
  "Set the account for composing a message by looking at the maildir"
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

;; headers in the overview
(setq mu4e-headers-fields
  '((:maildir       .  24)
    (:date          .  24)
    (:flags         .   6)
    (:from          .  24)
    (:subject       .  nil)))

;; bookmarks
(setq mu4e-bookmarks 
  '(("flag:unread AND NOT maildir:/gibbon/trash AND NOT maildir:/wunki/trash AND NOT maildir:/bread-and-pepper/trash" "All new messages" ?u)
    ("maildir:/gibbon/inbox"                      "Gibbon's inbox"         ?g)
    ("maildir:/wunki/inbox"                       "Wunki's inbox"          ?w)
    ("maildir:/bread-and-pepper/inbox"            "Bread & Pepper's inbox" ?b)
    ("date:today..now"                            "Today's messages"       ?t)
    ("flag:flagged"                               "Flagged messages"       ?f)))

;; shortcuts
(setq mu4e-maildir-shortcuts
       '(("/wunki/inbox"              . ?i)
         ("/bread-and-pepper/inbox"   . ?I)
         ("/wunki/archive"            . ?a)
         ("/bread-and-pepper/archive" . ?A)
         ("/wunki/sent"               . ?s)
         ("/bread-and-pepper/sent"    . ?S)
         ("/wunki/trash"              . ?t)
         ("/bread-and-pepper/trash"   . ?T)))

(provide 'wunki-mu4e)
