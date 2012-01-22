;;; bbdb-pgp.el --- use BBDB to store PGP preferences

;; Copyright (C) 1997,1999 Kevin Davidson

;; Author: Kevin Davidson tkld@quadstone.com
;; Maintainer: Kevin Davidson tkld@quadstone.com
;; Created: 10 Nov 1997
;; Version: $Revision: 1.6 $
;; Keywords: PGP BBDB message mailcrypt


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to tkld@quadstone.com) or
;; from the Free Software Foundation, Inc.,59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; LCD Archive Entry:
;; bbdb-pgp|Kevin Davidson|tkld@quadstone.com
;; |Use BBDB to store PGP preferences
;; |$Date: 2003/08/11 08:54:35 $|$Revision: 1.6 $|~/packages/bbdb-pgp.el

;;; Commentary:
;;
;; It is believed that encrypted mail works best if all mail between
;; individuals is encrypted - even concerning matters that are not
;; confidential. The reasoning is that confidential messages cannot
;; then be easily spotted and decryption efforts concentrated on them.
;; Some people therefore prefer to have all their email encrypted.
;; This package allows you to mark the BBDB entries for those
;; individuals so that messages will be encrypted when they are sent.
;;
;; These packages are required: BBDB, mailcrypt, message
;;
;; message.el is included with recent versions of Emacs.
;; You can use mail-mode as well as message-mode to send mail.

;;; Usage:
;; (require 'bbdb-pgp)
;;
;; Then for all users who you want to send encrypted mail to, add the field
;; pgp-mail with the value `encrypt'. Alternatively you can add the value
;; `sign' if you just want to send signed messages.
;;
;; and possibly (if you do not want the PGP field printed out)
;; (add-hook 'bbdb-print-elide bbdb-pgp-field)
;;
;; The variable bbdb/pgp-default-action defines what to do if the recipient
;; is not in the BBDB.

;;; TODO
;; Spot incoming PGP mail by hooking into mc-verify/decrypt and adding pgp-mail
;; field to BBDB entry (creating one if necessary); like bbdb-sc.el maintains
;; attribution prefs.

;;; PGP Public Key
;; The author's public key is available from any public PGP keyserver
;; eg http://www.pgp.net/pgpnet/
;; Fingerprint: 1F A9 3F 3E 90 F7 85 64  55 35 32 C8 75 91 3A E3


;;; Code:

(require 'message)
(require 'bbdb)
(condition-case nil (require 'mailcrypt) (error nil))

(defconst bbdb/pgp-version (substring "$Revision: 1.6 $" 11 -2)
  "$Id: bbdb-pgp.el,v 1.6 2003/08/11 08:54:35 waider Exp $

Report bugs to: Kevin Davidson tkld@quadstone.com")

;;;###autoload
(defgroup bbdb-utilities-pgp nil
  "Automatically sign and/or encrypt outgoing messages."
  :link '(emacs-library-link :tag "Lisp Source File" "bbdb-pgp.el")
  :group 'bbdb-utilities)


(defcustom bbdb/pgp-field 'pgp-mail
  "*Field to use in BBDB to store PGP preferences.

If this field's value in a record is \"encrypt\" then messages are
encrypted. If it is \"sign\" then messages are signed."
  :type 'symbol
  :tag "BBDB Field"
  :require 'bbdb
  :group 'bbdb-utilities-pgp)

(defcustom bbdb/pgp-method 'mailcrypt
  "*How to sign or encrypt messages.

'mailcrypt     means use Mailcrypt.
'mml-pgp       means add MML tags for Message to use old PGP format
'mml-pgpmime   means add MML tags for Message to use PGP/MIME
'mml-smime     means add MML tags for Message to use S/MIME"
  :type '(choice
      (const :tag "Mailcrypt" mailcrypt :require 'mailcrypt)
      (const :tag "MML PGP" mml-pgp :require 'mml)
      (const :tag "MML PGP/MIME" mml-pgpmime :require 'mml)
      (const :tag "MML S/MIME" mml-smime :require 'mml))
  :tag "Signing/Encryption Method"
  :group 'bbdb-utilities-pgp)

(defcustom bbdb/pgp-default-action nil
  "*Default action when sending a message and the recipient is not in BBDB.

nil         means do nothing.
'encrypt    means encrypt message.
'sign       means sign message."
  :type '(choice
      (const :tag "Do Nothing")
      (const :tag "Encrypt" encrypt)
      (const :tag "Sign" sign))
  :tag "Default Action"
  :group 'bbdb-utilities-pgp)

(defcustom bbdb/pgp-quiet nil
  "*Do not ask for confirmation on pgp-action.

nil         means normal messages/questions.
't          means to be quiet."
  :type '(choice
      (const :tag "normal")
      (const :tag "quiet" t))
  :tag "Quietness"
  :group 'bbdb-utilities-pgp)

(defun bbdb/pgp-get-pgp (name address)
  "Look up user NAME and ADDRESS in BBDB and return the PGP preference."
  (let* ((record (bbdb-search-simple name address))
     (pgp (and record
           (bbdb-record-getprop record bbdb/pgp-field))))
    pgp))

(defun bbdb/pgp-sign ()
  "Sign a message.
bbdb/pgp-method controls the method used."
  (cond
   ((eq bbdb/pgp-method 'mailcrypt)
    (mc-sign 0))
   ((eq bbdb/pgp-method 'mml-pgp)
    (mml-secure-message-sign-pgp))
   ((eq bbdb/pgp-method 'mml-pgpmime)
    (mml-secure-message-sign-pgpmime))
   ((eq bbdb/pgp-method 'mml-smime)
    (mml-secure-message-sign-smime))
   (t
    (error 'invalid-state "bbdb/pgp-method"))))

(defun bbdb/pgp-encrypt ()
  "Encrypt and sign a message.
bbdb/pgp-method controls the method used."
  (cond
   ((eq bbdb/pgp-method 'mailcrypt)
    (mc-encrypt 0))
   ((eq bbdb/pgp-method 'mml-pgp)
    (mml-secure-message-encrypt-pgp))
   ((eq bbdb/pgp-method 'mml-pgpmime)
    (mml-secure-message-encrypt-pgpmime))
   ((eq bbdb/pgp-method 'mml-smime)
    (mml-secure-message-encrypt-smime))
   (t
    (error 'invalid-state "bbdb/pgp-method"))))

(defun bbdb/pgp-hook-fun ()
  "Function to be added to message-send-hook
Uses PGP to encrypt messages to users marked in the BBDB with the
field `bbdb/pgp-field'.
The user is prompted before encryption or signing."
  (save-restriction
    (save-excursion
      (message-narrow-to-headers)
      (and (featurep 'mailalias)
       (not (featurep 'mailabbrev))
       mail-aliases
       (expand-mail-aliases (point-min) (point-max)))
      (let* ((to-field (mail-fetch-field "To" nil t))
         (address (mail-extract-address-components (or to-field ""))))
    (widen)
    (if (not (equal address '(nil nil)))
        (let ((pgp-p (bbdb/pgp-get-pgp (car address) (car (cdr address)))))
          (cond
           ((string= "encrypt" pgp-p)
                (and (or bbdb/pgp-quiet
                         (y-or-n-p "Encrypt message? "))
                     (bbdb/pgp-encrypt)))
           ((string= "sign" pgp-p)
        (and (or bbdb/pgp-quiet
                         (y-or-n-p "Sign message? "))
                     (bbdb/pgp-sign)))
           (t
        (cond
         ((eq bbdb/pgp-default-action 'encrypt)
          (and (y-or-n-p "Encrypt message? ")
               (bbdb/pgp-encrypt)))
         ((eq bbdb/pgp-default-action 'sign)
          (and (y-or-n-p "Sign message? ")
               (bbdb/pgp-sign)))
         (t
          nil))))))))))

(add-hook 'message-send-hook 'bbdb/pgp-hook-fun)
(add-hook 'mail-send-hook 'bbdb/pgp-hook-fun)

(provide 'bbdb-pgp)

;;; bbdb-pgp.el ends here
