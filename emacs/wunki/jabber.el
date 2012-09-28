;; simpler format for prompts
(setq jabber-chat-buffer-show-avatar nil
      jabber-chat-foreign-prompt-format "> "
      jabber-chat-local-prompt-format "> "
      jabber-chat-system-prompt-format "*** "
      jabber-chat-time-format "%H:%M"
      jabber-default-show ""
      jabber-groupchat-prompt-format "%n> "
      jabber-muc-private-foreign-prompt-format "%g/%n> ")

;; better defaults
(setq jabber-backlog-days 3.0
      jabber-roster-line-format "%c %-25n %u %-8s"
      jabber-roster-show-title nil
      jabber-show-resources nil
      jabber-show-offline-contacts nil)

;; colors
(custom-set-faces
 '(jabber-chat-prompt-foreign ((t (:foreground "red"))))
 '(jabber-chat-prompt-local ((t (:foreground "blue"))))
 '(jabber-chat-prompt-system ((t (:foreground "dark green" :weight bold))))
 '(jabber-roster-user-away ((t (:foreground "orange"))))
 '(jabber-roster-user-chatty ((t (:foreground "green"))))
 '(jabber-roster-user-online ((t (:foreground "dark green")))))

(defun start-jabber ()
  "Wrapper for starting jabber because we need the passwords"
  (interactive)
  (require 'secrets "wunki/secrets.el")
  ;; default nicknames
  (setq jabber-muc-default-nicknames 
        `((,jabber-doo-username . "Petar Radosevic")
          (,jabber-bp-username . "Petar Radosevic")))

  ;; jabber accounts
  (setq jabber-account-list
        `((,jabber-doo-username
           (:network-server . "im.doo.net")
           (:password . ,jabber-doo-password))
          (,jabber-bp-username
           (:password . ,jabber-bp-password)
           (:network-server . "talk.google.com")
           (:connection-type . ssl))))
  (jabber-connect-all))
(global-set-key (kbd "C-c j") 'start-jabber)
