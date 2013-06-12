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

;; only show notifications in the modeline when my name is used
(setq erc-current-nick-highlight-type 'nick)
(setq erc-keywords '("\\wunki[-a-z]*\\b" "\\petar[-a-z]*\\b"))
(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq erc-track-use-faces t)
(setq erc-track-exclude-server-buffer t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face))
(setq erc-track-priority-faces-only 'all)

;; automagically resize the window
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

;; NickServ
(require 'erc-services)

(setq erc-prompt-for-nickserv-password nil)

;; starts the ERC server or switches to it.
(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "wunki.org:7000")         ;; ERC already active?
      (erc-track-switch-buffer 1)           ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ")          ;; no: maybe start ERC
      (require 'secrets "wunki/secrets.el") ;; load passwords
      (erc :server "wunki.org"
           :port 7000
           :nick "wunki"
           :password irc-wunki
           :full-name "Petar Radosevic"))))
