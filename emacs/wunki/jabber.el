(setq jabber-account-list
    `((,secret-jabber-doo-username
       (:network-server . "im.doo.net")
       (:password . ,secret-jabber-doo-password))
      (,secret-jabber-bp-username
       (:password . ,secret-jabber-bp-password)
       (:network-server . "talk.google.com")
       (:connection-type . ssl))))
