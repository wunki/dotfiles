;; dynamically generate the temporary file directory
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))

;; make the temporary directory if not available
(make-directory user-temporary-file-directory t)

;; use copying to create backup files
(setq backup-by-copying t)

;; set the directory
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)))

(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))

(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))
