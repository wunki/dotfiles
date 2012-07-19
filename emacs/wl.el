;; mode:-*-emacs-lisp-*-
;; wanderlust settings

(setq elmo-maildir-folder-path "~/Mail"          ;; where i store my mail
      wl-stay-folder-window t                    ;; show the folder pane (left)
      wl-folder-window-width 25                  ;; toggle on/off with 'i'
      wl-fcc-force-as-read t
      wl-default-folder ".Wunki/Inbox"
      wl-temporary-file-directory "~/.tmp/"
      wl-user-mail-address-list (quote ("petar@wunki.org" "petar@breadandpepper.com")))

;; send mail with msmtp
(setq wl-draft-send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtpq"
      message-send-mail-function 'message-send-mail-with-sendmail
      mail-specify-envelope-from t
      message-sendmail-f-is-evil nil
      mail-envelope-from 'header
      mail-interactive nil
      message-sendmail-envelope-from 'header)

;; templates
(setq wl-template-alist
      '(("default"
         (wl-from . "Petar Radosevic <petar@wunki.org>")
         ("From" . wl-from)
         (signature-file-name . "~/.signature-wunki")
         (wl-smtp-posting-user . "petar@wunki.org")
         
         (wl-fcc . ".Wunki/Sent")
         (wl-draft-folder . ".Wunki/Drafts")
         (wl-trash-folder . ".Wunki/Trash")
         (wl-queue-folder . ".Wunki/Queue"))

        ("bread-and-pepper"
         (wl-from . "Petar Radosevic <petar@breadandpepper.com>")
         ("From" . wl-from)
         (signature-file-name . "~/.signature-bp")
         (wl-smtp-posting-user . "petar@breadandpepper.com")
         (wl-smtp-posting-server . "mail.company.com")
         (wl-local-domain . "breadandpepper.com")

         ("Fcc" . ".Bread & Pepper/Sent")
         (wl-draft-folder . ".Bread & Pepper/Drafts")
         (wl-trash-folder . ".Bread & Pepper/Trash")
         (wl-queue-folder . ".Bread & Pepper/Queue"))))

;; automatically select the correct template based on which folder I'm visiting
(setq wl-draft-config-matchone t)

;; choose the correct template
(setq wl-draft-config-alist
          '(((string-match ".*Wunki" wl-draft-parent-folder)
             (template . "default"))
            ((string-match ".*Bread" wl-draft-parent-folder)
             (template . "bread-and-pepper"))))

;; delete messages
(setq wl-dispose-folder-alist
      '((".*Trash" . remove)
        (".*Bread.*" . ".Bread & Pepper/Trash")
        (".*Wunki.*" . ".Wunki/Trash")))

;; hide many fields from message buffers
(setq wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
          '("^\\(To\\|Cc\\):"
            "^Subject:"
            "^\\(From\\|Reply-To\\):"
            "^Organization:"
            "^Message-Id:"
            "^\\(Posted\\|Date\\):")
       wl-message-sort-field-list
           '("^From"
             "^Organization:"
             "^X-Attribution:"
             "^Subject"
             "^Date"
             "^To"
             "^Cc"))

;; Apply wl-draft-config-alist as soon as you enter in a draft buffer. Without
;; this wanderlust would apply it only when actually sending the e-mail.
(add-hook 'wl-mail-setup-hook 'wl-draft-config-exec)

;; choose the correct send method
(defun choose-msmtp-account ()
  (setq message-sendmail-extra-arguments (if (equal "petar@wunki.org" wl-smtp-posting-user)
                                             '("-a" "wunki")
                                           '("-a" "breadandpepper"))))
(add-hook 'wl-draft-send-hook 'choose-msmtp-account)


;; cycle through templates with arrow keys
(define-key wl-template-mode-map (kbd "<right>") 'wl-template-next)
(define-key wl-template-mode-map (kbd "<left>") 'wl-template-prev)
