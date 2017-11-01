(require 'f)

;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))
(setq eshell-banner-message "")

;; Disable line highligthing
(add-hook 'eshell-mode-hook (lambda ()
                              (setq-local global-hl-line-mode nil)
                              (define-key eshell-mode-map (kbd "M-p") 'ace-window)))

;; Prompt with a bit of help from http://www.emacswiki.org/emacs/EshellPrompt
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun eshell/abbr-pwd ()
  (let ((home (getenv "HOME"))
        (path (eshell/pwd)))
    (cond
     ((string-equal home path) "~")
     ((f-ancestor-of? home path) (concat "~/" (f-relative path home)))
     (path))))

(defun eshell/my-prompt ()
  (let ((header-bg "#161616"))
    (concat
     (with-face user-login-name :foreground "#c991e1")
     (with-face (concat "@" system-name) :foreground "#906cff")
     " "
     (with-face (eshell/abbr-pwd) :foreground "#5fd787")
     (if (= (user-uid) 0)
         (with-face "#" :foreground "#ff5458")
       (with-face "$" :foreground "#65b2ff"))
     " ")))

(setq eshell-prompt-function 'eshell/my-prompt)
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-regexp "^[^#$\n]+[#$] ")

(setq eshell-cmpl-cycle-completions nil)

;; quickly fire up e-shell.
(global-set-key (kbd "C-c C-e") 'eshell)

(provide 'wunki-eshell)
