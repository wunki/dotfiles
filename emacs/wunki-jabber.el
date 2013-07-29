;; simpler format for prompts
(setq jabber-chat-buffer-show-avatar nil
      jabber-chat-foreign-prompt-format "> "
      jabber-chat-local-prompt-format "> "
      jabber-chat-system-prompt-format "*** "
      jabber-chat-time-format "%H:%M"
      jabber-default-show ""
      jabber-groupchat-prompt-format "%n> "
      jabber-muc-private-foreign-prompt-format "%g/%n> ")

;; improve the default settings
(setq jabber-roster-line-format "%c %-25n %u %-8s"
      jabber-roster-show-title nil
      jabber-show-resources nil
      jabber-show-offline-contacts nil
      jabber-avatar-cache-directory "/tmp/jabber-avatars"
      jabber-username "Petar Radosevic"
      jabber-events-request-these (quote (delivered displayed composing)))

;; don't show any presence notifications
(setq jabber-alert-presence-hooks '())

;; history
(setq jabber-history-enabled t
      jabber-use-global-history nil
      jabber-backlog-number 40
      jabber-backlog-days 30
      jabber-history-dir "~/.cache/jabber-history")

;; To join HipChat rooms easily
(defvar hipchat-number "39101")
(defvar hipchat-nickname "Petar Radosevic")
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (jabber-read-account)
   (concat hipchat-number "_" room "@conf.hipchat.com")
   hipchat-nickname
   t))

(defun jabber-start ()
  "wrapper for starting jabber because we need the passwords"
  (interactive)
  (require 'wunki-secrets)
  ;; jabber accounts
  (setq jabber-account-list
        `((,jabber-hipchat-username
           (:network-server . "chat.hipchat.com")
           (:password . ,jabber-hipchat-password)
           (:connection-type . ssl))))
  (jabber-connect-all))

(provide 'wunki-jabber)
