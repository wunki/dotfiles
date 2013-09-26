;; add some extra exect paths
(add-to-list 'exec-path "/home/wunki/bin/")
(add-to-list 'exec-path "/home/wunki/.local/bin/")
(add-to-list 'exec-path "/home/wunki/.cabal/bin/")
(setq exec-path (append exec-path '("/usr/local/bin")))

(provide 'wunki-shell)
