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

;; join rooms
(setq jabber-muc-autojoin '("server@conference.im.doo.net"))

(defun jabber-start ()
  "wrapper for starting jabber because we need the passwords"
  (interactive)
  (require 'secrets "wunki/secrets.el")
  ;; default nicknames
  (setq jabber-muc-default-nicknames 
        `(("server@conference.im.doo.net" . "Petar Radosevic")))

  ;; jabber accounts
  (setq jabber-account-list
        `((,jabber-doo-username
           (:network-server . "im.doo.net")
           (:password . ,jabber-doo-password))))
  (jabber-connect-all))
