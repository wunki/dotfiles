;; load theme and don't ask if it's save.
(load-theme 'sanityinc-tomorrow-blue t)

(set-frame-font "Ubuntu Mono")
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono"))
(set-face-attribute 'default nil :height 150)

(defun toggle-dark-light-theme ()
  "Switch between dark and light theme."
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'dark)
      (load-theme 'sanityinc-tomorrow-bright)
    (load-theme 'zenburn)))

;; Stolen from Sanity inc
(defun wunki/font-name-replace-size (font-name new-size)
  (let ((parts (split-string font-name "-")))
    (setcar (nthcdr 7 parts) (format "%d" new-size))
    (mapconcat 'identity parts "-")))

(defun wunki/increment-default-font-height (delta)
  "Adjust the default font height by DELTA on every frame.
Emacs will keep the pixel size of the frame approximately the
same. DELTA should be a multiple of 10, to match the units used
by the :height face attribute."
  (let* ((new-height (+ (face-attribute 'default :height) delta))
         (new-point-height (/ new-height 10)))
    (dolist (f (frame-list))
      (with-selected-frame f
        ;; Latest 'set-frame-font supports a "frames" arg, but
        ;; we cater to Emacs 23 by looping instead.
        (set-frame-font (wunki/font-name-replace-size
                         (face-font 'default)
                         new-point-height)
                        t)))
    (set-face-attribute 'default nil :height new-height)
    (message "default font size is now %d" new-point-height)))

(defun wunki/increase-default-font-height ()
  (interactive)
  (wunki/increment-default-font-height 10))

(defun wunki/decrease-default-font-height ()
  (interactive)
  (wunki/increment-default-font-height -10))

(global-set-key (kbd "C-M-=") 'wunki/increase-default-font-height)
(global-set-key (kbd "C-M--") 'wunki/decrease-default-font-height)

(provide 'wunki-theme)
