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
(setq jabber-backlog-days 3.0
      jabber-roster-line-format "%c %-25n %u %-8s"
      jabber-roster-show-title nil
      jabber-show-resources nil
      jabber-show-offline-contacts nil
      jabber-avatar-cache-directory "/tmp/jabber-avatars"
      jabber-username "Petar Radosevic")

;; colors
(custom-set-faces
 '(jabber-chat-prompt-foreign ((t (:foreground "red"))))
 '(jabber-chat-prompt-local ((t (:foreground "blue"))))
 '(jabber-chat-prompt-system ((t (:foreground "dark green" :weight bold))))
 '(jabber-roster-user-away ((t (:foreground "orange"))))
 '(jabber-roster-user-chatty ((t (:foreground "green"))))
 '(jabber-roster-user-online ((t (:foreground "dark green")))))

;; join rooms
(setq jabber-muc-autojoin '("server@conference.im.doo.net"))

(defun egh:jabber-google-groupchat-create ()
      (interactive)
      (let ((group (apply 'format "private-chat-%x%x%x%x%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x%x%x%x%x%x%x%x%x@groupchat.google.com"
                          (mapcar (lambda (x) (random x)) (make-list 32 15))))
            (account (jabber-read-account)))
        (jabber-groupchat-join account group (jabber-muc-read-my-nickname account group) t)))

(defun jabber-start ()
  "Wrapper for starting jabber because we need the passwords"
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
