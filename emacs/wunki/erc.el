(require 'erc)

;; check channels, not interested in hearing about these events
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

; automagically resize the window
(add-hook 'window-configuration-change-hook 
	   '(lambda ()
	      (setq erc-fill-column (- (window-width) 2))))

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "wunki.org:7000") ;; ERC already active?
      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (require 'secrets "wunki/secrets.el")
      (erc :server "wunki.org"
           :port 7000 
           :nick "wunki" 
           :full-name "Petar Radosevic"
           :password erc-wunki))))

;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c C-e") 'erc-start-or-switch)
