(require 'gnus)

; hotkey for gnus
(define-key global-map (kbd "<f7>") 'gnus)

; general settings
(setq gnus-directory "~/.gnus/news/"
      gnus-article-save-directory "~/.gnus/news/"

      ;; annoying mail folders
      message-auto-save-directory "~/.gnus/archive"
      nndraft-directory "~/.gnus/archive"

      gnus-message-archive-method
      '(nnfolder "archive"
                 (nnfolder-inhibit-expiry t)
                 (nnfolder-active-file "~/.gnus/archive/active")
                 (nnfolder-directory "~/.gnus/archive")))

; mail servers
(setq gnus-select-method
      '(nnimap "wunki"
               (nnimap-address "127.0.0.1")
               (nnimap-stream network)
               (nnimap-authenticator login)))

(setq gnus-secondary-select-methods
      '((nnimap "breadandpepper"
                (nnimap-address "arch.wunki.org")
                (nnimap-stream network)
                (nnimap-authenticator login))))

; archiving the messages
(setq gnus-message-archive-group
           '(("wunki" "nnimap+wunki:sent")
             ("breadandpepper" nil)))

(setq gnus-parameters
      '(("wunki" 
         (gcc-self . "nnimap+wunki:sent"))
        ("breadandpepper"
         (gcc-self . nil))))

; identification
(setq gnus-posting-styles
      '((".*"
         (name "Petar Radosevic"))
        ("INBOX.*"
         (address "petar@wunki.org")
         (organization "Wunki")
         (signature-file "~/.gnus/signature-wunki")
         (x-url "https://www.wunki.org")
         (eval (setq message-sendmail-extra-arguments '("-a" "wunki"))))
        (".+breadandpepper.+"
         (address "petar@breadandpepper.com")
         (organization "Bread & Pepper")
         (signature-file "~/.gnus/signature-bp")
         (x-url "http://www.breadandpepper.com")
         (eval (setq message-sendmail-extra-arguments '("-a" "breadandpepper"))))))

; spell check on sent
(add-hook 'message-send-hook 'ispell-message) ; spell check on sent

; don't ignore groups
(setq gnus-ignored-newsgroups ""
      gnus-subscribe-newsgroup-method (quote gnus-subscribe-topics))

; don't ask me how much to download
(setq gnus-large-newsgroup 'nil)

; bbdb
(add-to-list 'load-path "~/.emacs.d/vendor/bbdb-2.35/lisp/")
(require 'bbdb)
(require 'bbdb-autoloads)
(bbdb-initialize)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq
 bbdb-file "~/.gnus/contacts/bbdb"
 bbdb-offer-save 'auto
 bbdb-complete-name-full-completion t
 bbdb-completion-type 'primary-or-name
 bbdb-notice-auto-save-file t
 bbdb-expand-mail-aliases t
 bbdb-canonicalize-redundant-nets-p t
 bbdb-always-add-addresses t
 bbdb-complete-name-allow-cycling t
 bbdb-send-mail-style 'gnus
 ;; make bbdb really small
 bbdb-use-pop-up t
 bbdb-electric-p t
 bbdb-popup-target-lines 1)

; show headings for when using multiple mail boxes
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

; I prefer reading e-mail in plain text
(eval-after-load "mm-decode"
 '(progn 
      (add-to-list 'mm-discouraged-alternatives "text/html")
      (add-to-list 'mm-discouraged-alternatives "text/richtext")))

; warn me if I try to reply in a newsgroup
(setq gnus-confirm-mail-reply-to-news t)

; sending mail
(setq sendmail-program "/home/wunki/.bin/msmtpq"
      message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t
      message-sendmail-f-is-evil nil                
      mail-envelope-from 'header
      ;; don't wait for a response
      mail-interactive nil
      message-sendmail-envelope-from 'header)

; mark send messages as read
(setq gnus-gcc-mark-as-read t) 

; attractive summary view
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "◯ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "│─► ")
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
      mm-decrypt-option 'always)

; sign all my e-mails
; (add-hook 'gnus-message-setup-hook 'mml-secure-message-sign-pgpmime) ; sign all messages
