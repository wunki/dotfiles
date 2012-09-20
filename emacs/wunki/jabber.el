(setq jabber-account-list
    `((,jabber-doo-username
       (:network-server . "im.doo.net")
       (:password . ,jabber-doo-password))
      (,jabber-bp-username
       (:password . ,jabber-bp-password)
       (:network-server . "talk.google.com")
       (:connection-type . ssl))))

(setq jabber-muc-default-nicknames 
      `((,jabber-doo-username . "Petar Radosevic")
        (,jabber-bp-username . "Petar Radosevic")))
