(setq jabber-account-list
    `((,secret-jabber-doo-username
       (:network-server . "im.doo.net")
       (:password . ,secret-jabber-doo-password))
      (,secret-jabber-bp-username
       (:password . ,secret-jabber-bp-password)
       (:network-server . "talk.google.com")
       (:connection-type . ssl))))

(setq jabber-muc-default-nicknames 
      `((,secret-jabber-doo-username . "Petar Radosevic")
        (,secret-jabber-bp-username . "Petar Radosevic")))
