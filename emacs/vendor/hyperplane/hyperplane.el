;; hyperplane.el
;;
;; Copyright 2010 John Olsson (john@cryon.se)

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;; ---------
;; Changelog
;; ---------
;; 2010-11-16
;;  * added calender faces
;; 2010-11-12
;;  * added ansi-colors for term
;;  * added palette
;;  * cleaned up
;; 2010-11-10
;;  * initial version

(require 'color-theme)

;; the colors!
(defvar hp-red           "#cc3030")
(defvar hp-dark-orange   "#cc6e51")
(defvar hp-beige         "#bba47c")
(defvar hp-orange        "#ccad42")
(defvar hp-yellow        "#c4d249")
(defvar hp-green         "#7bbf11")
(defvar hp-dark-green    "#3ab145")
(defvar hp-cyan          "#6ccccb")
(defvar hp-blue          "#4e9ecc")
(defvar hp-baby-blue     "#80a0cc")
(defvar hp-gray          "#8f90a4")
(defvar hp-dark-gray     "#767790")
(defvar hp-purple        "#9072b3")
(defvar hp-cerise        "#b33fb2")
(defvar hp-light-gray    "#b7c3cd")
(defvar hp-white         "#dedee0")

(defvar hp-total-red     "#ff0000")
(defvar hp-total-green   "#00ff00")
(defvar hp-total-blue    "#0000ff")
(defvar hp-total-purple  "#a020f0")
(defvar hp-total-cyan    "#00ffff")
(defvar hp-total-yellow  "#ffff00")
(defvar hp-total-white   "#ffffff")
(defvar hp-total-black   "#000000")

; the elements
(defvar hp-background    "#3a3f46")
(defvar hp-background-1  "#373b42")
(defvar hp-background-2  "#32363c")

(defvar hp-foreground    hp-light-gray)
(defvar hp-comment       hp-dark-gray)
(defvar hp-identifier    hp-baby-blue)
(defvar hp-string        hp-green)
(defvar hp-preprocessor  hp-dark-orange)
(defvar hp-builtin       hp-orange)
(defvar hp-constant      hp-white)
(defvar hp-doc           hp-purple) ; for the ladies!

(defun color-theme-hyperplane ()
  "Sets up the hyperplane color-theme theme"
  (interactive)
  (color-theme-install
   `(color-theme-hyperplane
     ((foreground-color . ,hp-foreground)
      (background-color . ,hp-background)
      (background-mode  . dark))
     
     (cursor
      ((t (:background ,hp-white))))

     (fringe
      ((t (:background ,hp-background))))

     (region
      ((t (:background ,hp-dark-gray :foreground ,hp-background-2))))

     (highlight
      ((t (:inherit region))))

     (modeline
      ((t (:background ,hp-background-2 :foreground ,hp-foreground))))

     (modeline-inactive
      ((t (:background ,hp-background-1 :foreground ,hp-dark-gray))))

     (minibuffer-prompt
      ((t (:bold t :foreground ,hp-white))))

     (show-paren-match-face
      ((t (:foreground ,hp-total-green :bold t))))

     (show-paren-mismatch-face
      ((t (:foreground ,hp-total-red :bold t))))

     (isearch
      ((t (:foreground ,hp-total-green :bold t))))
     
     ;; ---------
     ;; font-lock
     ;; ---------
     (font-lock-comment-face
      ((t (:foreground ,hp-comment))))

     (font-lock-string-face
      ((t (:foreground ,hp-string))))

     (font-lock-variable-name-face
      ((t (:foreground ,hp-identifier))))

     (font-lock-type-face
      ((t (:foreground ,hp-foreground :bold t))))

     (font-lock-keyword-face
      ((t (:bold t :foreground ,hp-identifier))))

     (font-lock-function-name-face
      ((t (:foreground ,hp-identifier))))

     (font-lock-preprocessor-face
      ((t (:foreground ,hp-preprocessor))))

     (font-lock-constant-face
      ((t (:foreground ,hp-constant))))

     (font-lock-builtin-face
      ((t (:foreground ,hp-builtin))))

     (font-lock-warning-face
      ((t (:foreground ,hp-red :bold t))))

     (font-lock-doc-face
      ((t (:foreground ,hp-doc))))

     (font-lock-negation-char-face
      ((t (:foreground ,hp-orange :bold t))))

     (font-lock-comment-delimiter-face
      ((t (:inherit font-lock-comment-face))))

     ;; FIX: have not tested this yet
     (font-lock-reference-face
      ((t (:bold t :foreground ,hp-total-yellow))))

     ;; ------
     ;; eshell
     ;; ------
     (eshell-ls-clutter
      ((t (:inherit font-lock-comment-face))))

     (eshell-ls-executable
      ((t (:foreground ,hp-string))))

     (eshell-ls-directory
      ((t (:foreground ,hp-baby-blue :bold t))))

     (eshell-ls-archive
      ((t (:foreground ,hp-purple))))

     (eshell-ls-backup
      ((t (:inherit font-lock-comment-face))))

     (eshell-ls-missing
      ((t (:inherit font-lock-warning-face))))

     (eshell-ls-unreadable
      ((t (:inherit font-lock-warning-face))))

     (eshell-ls-symlink
      ((t (:inherit font-lock-builtin-face))))

     (eshell-prompt
      ((t (:foreground ,hp-light-gray :bold t))))

     ;; ---
     ;; erc
     ;; ---
     (erc-default-face
      ((t (:foreground ,hp-foreground))))

     (erc-current-nick-face
      ((t (:inherit erc-keyword-face))))

     (erc-action-face
      ((t (:inherit erc-default-face))))

     (erc-dangerous-host-face
      ((t (:inherit font-lock-warning-face))))

     (erc-highlight-face
      ((t (:foreground ,hp-orange))))

     (erc-direct-msg-face
      ((t (:foreground ,hp-foreground))))

     (erc-nick-msg-face
      ((t (:foreground ,hp-white))))

     (erc-fool-face
      ((t (:forground ,hp-dark-gray))))

     (erc-input-face
      ((t (:foreground ,hp-white))))

     (erc-error-face
      ((t (:inherit font-lock-warning-face))))

     (erc-keyword-face
      ((t (:background ,hp-background-2))))

     (erc-nick-default-face
      ((t (:foreground ,hp-white))))

     (erc-prompt-face
      ((t (:inherit eshell-prompt))))

     (erc-notice-face
      ((t (:foreground ,hp-dark-gray))))

     (erc-timestamp-face
      ((t (:foreground ,hp-dark-gray))))

     (erc-pal-face
      ((t (:foreground ,hp-green))))

     ;; FIX: doesn't seem to work...
     (erc-header-line
      ((t (:background ,hp-background-2 :foreground ,hp-purple))))

     ;; --------
     ;; calendar
     ;; --------
     (calendar-today-face
      ((t (:foreground ,hp-green :bold t))))

     (holiday-face
      ((t (:foreground ,hp-orange))))
     
     (diary-face
      ((t (:foreground ,hp-purple))))
     
     ;; ---
     ;; nav
     ;; ---
     (nav-face-heading
      ((t (:foreground ,hp-green))))

     (nav-face-dir
      ((t (:inherit eshell-ls-directory))))

     (nav-face-hdir
      ((t (:inherit nav-face-dir))))

     (nav-face-button-num
      ((t (:inherit eshell-prompt))))

     (nav-face-file
      ((t (:foreground ,hp-foreground))))

     ;; FIX: have not tested this yet
     (nav-face-hfile
      ((t (:foreground ,hp-orange))))

)))

;; colors for term
(eval-after-load 'term
  '(setq ansi-term-color-vector
         (vector 'unspecified 
		 hp-dark-gray
                 hp-red 
		 hp-green
                 hp-orange
		 hp-baby-blue
                 hp-purple 
		 hp-cyan)))

(provide 'hyperplane)
