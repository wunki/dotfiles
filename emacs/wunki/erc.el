(require 'erc)

;; check channels, not interested in hearing about these events
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "wunki.org:7000") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "wunki.org" :port 7000 :nick "wunki" :full-name "Petar Radosevic"))))

;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e") 'erc-start-or-switch) ;; ERC
