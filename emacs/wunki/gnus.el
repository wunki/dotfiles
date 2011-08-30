(require 'gnus)

; identification
(setq gnus-posting-styles
      '((".*"
         (name "Petar Radosevic")
         (address "petar@wunki.org")
         ("X-URL" "http://www.wunki.org"))))

; spell check on sent
(add-hook 'message-send-hook 'ispell-message) ; spell check on sent

; location of mail
(setq nnml-directory "~/Mail")
(setq message-directory "~/Mail")

; don't ignore groups
(setq gnus-ignored-newsgroups "")

; reading mail
(setq gnus-select-method 
      '(nnmaildir "wunki" 
                  (directory "~/Mail/")
                  (directory-files nnheader-directory-files-safe) 
                  (get-new-mail nil)))

; sending mail
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("secure.emailsrvr.com" 587 nil nil))
      smtpmail-auth-credentials '(("secure.emailsrvr.com" 587 "petar@wunki.org" nil))
      smtpmail-default-smtp-server "secure.emailsrvr.com"
      smtpmail-smtp-server "secure.emailsrvr.com"
      smtpmail-smtp-service 587
      starttls-use-gnutls t)

; sent messages
(setq gnus-gcc-mark-as-read t) 

; attractive summary view
; TODO why doesn't this character display │
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))


(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{%}" "%1{%d%}" "%3{%}" ;; date
       "  "
       "%4{%-20,20f%}"           ;; name
       "  "
       "%3{%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

; GPG settings
(setq mml2015-use 'epg)
(setq mml2015-encrypt-to-self t)
(setq mml2015-verbose t)
(setq mml2015-always-trust nil)
(setq mml2015-passphrase-cache-expiry '7200)

; add GPG to gnus
(add-hook 'message-setup-hook
          (lambda ()
            (if gnus-newsgroup-name
                (let ((signers (gnus-group-get-parameter
                                gnus-newsgroup-name
                                'mml2015-signers
                                t)))
                  (if signers
                      (set (make-local-variable 'mml2015-signers)
                           signers))))))

; verify signatures
(setq gnus-message-replysign t
      gnus-message-replyencrypt t
      gnus-message-replysignencrypted t
      gnus-treat-x-pgp-sig t
      mm-verify-option 'always
      mm-decrypt-option 'always
)

; sign all my e-mails
(add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime) ; sign all messages

