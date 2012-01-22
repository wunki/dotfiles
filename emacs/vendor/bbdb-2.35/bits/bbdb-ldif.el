;;; Copyright (C) 1998,2000 by Niels Elgaard Larsen <elgaard@diku.dk>

;;; $Log: bbdb-ldif.el,v $
;;; Revision 1.1  2005/02/13 14:16:03  waider
;;; * added new file, with minor abuse to make it work with current BBDB
;;;
;;; Revision 1.7  2000/03/15 14:16:44  elgaard
;;; Fixed problem with concatenation of strings/integers
;;; Changed mobiletelephonenumber to cellphone to follow Netscape :-(
;;; Added support for pagerphone
;;;
;;; Revision 1.6  1998/09/08 12:35:27  elgaard
;;; Works with xemacs, emacs, emacs-19.34, bbdb-2 and bbdb-1.51
;;; Bugfixes
;;;
;; Rev 0.3
;; Can export mail-alias'es and .mailrc aliases to Netscape Mailing List
;;Bugfix.
;;

;; Rev. 0.2.1
;; Compiles without MEL

;; Rev. 0.2
;; Notes work better now
;; added 'bbdb-elided-export-ldif'
;; Fixed base64 bug

;;     This program is free software; you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation; either version 2 of the License, or
;;     (at your option) any later version.

;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.

;;     You should have received a copy of the GNU General Public License
;;     along with this program; if not, write to the Free Software
;;     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Niels Elgaard Larsen, <URL:mailto:elgaard@diku.dk>
;; July 18, 1998

;; bbdb-import-ldif imports LDIF entries
;; bbdb-to-ldif export bbdb to LDIF.

;; Both functions are somewhat specialized for Netscape Communicator (and Mozilla)



;;; Installation:

;;; Put (add-hook 'bbdb-load-hook (function (lambda () (require 'bbdb-ldif))))
;;; into your .emacs, or autoload it.


;; If you use non-ASCII characters recode the output file from emacs:
;;  "recode  ..UTF-8 output.ldif"
;; and the input file from Netscape:
;;  "recode  UTF-8.. i2.ldif "
;;;;;; Does not work for base-64 encoded text.

(require 'bbdb)

;; WAIDER MOD FEB 2005
;; deprecated functions. I should fix the code rather than do this, but.
(defun bbdb-address-street1(addr)
  (nth 0 (bbdb-address-streets addr)))
(defun bbdb-address-street2(addr)
  (nth 1 (bbdb-address-streets addr)))
(defun bbdb-address-street3(addr)
  (nth 2 (bbdb-address-streets addr)))

(if (locate-library "mel") (require 'mel)
  (message "We try without MEL (base64 operation), multiline fields will not work"
       )
  )

(if  (fboundp 'split-string) nil
  (defun split-string (string &optional pattern)
    "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
    (or pattern
    (setq pattern "[ \f\t\n\r\v]+"))
    ;; The FSF version of this function takes care not to cons in case
    ;; of infloop.  Maybe we should synch?
    (let (parts (start 0))
      (while (string-match pattern string start)
    (setq parts (cons (substring string start (match-beginning 0)) parts)
          start (match-end 0)))
      (nreverse (cons (substring string start) parts))))
  )

(if (fboundp 'caadr) nil (defun caadr (foo) (car (car (cdr foo)))))



(defvar bbdb-ldif-nsnil "?" "Null name for Netscape")

(defun tnsnil (st)
     (if (equal st bbdb-ldif-nsnil)
     nil
       st))

(defvar bbdb-elided-export-ldif nil "Set this to a list of some
of the symbols '(address phone net notes) to select those fields to be left
out when exporting to LDIF format"
)

;(require 'bbdb-snarf)
(require 'bbdb-com)


(defvar bbdb-ldif-prefix "xbbdb")
(defvar bbdb-ldif-prefixh "xhbbdb")

;;;; From bbdb-snarf with bugfix:
(defun bbdb-merge-internally-ldif (old-record new-record)
  "Merge two records.  NEW-RECORDS wins over OLD in cases of ties."
  (if (and (null (bbdb-record-firstname new-record))
       (bbdb-record-firstname old-record))
      (bbdb-record-set-firstname new-record (bbdb-record-firstname old-record)))
  (if (and (null (bbdb-record-lastname new-record))
       (bbdb-record-lastname old-record))
      (bbdb-record-set-lastname new-record (bbdb-record-lastname old-record)))
  (if (and (null (bbdb-record-company new-record))
       (bbdb-record-company old-record))
      (bbdb-record-set-company new-record (bbdb-record-company old-record)))
  ;; nets
  (let ((old-nets (bbdb-record-net old-record))
    (new-nets (bbdb-record-net new-record)))
    (while old-nets
      (if (not (member (car old-nets) new-nets))
      (setq new-nets (append new-nets (list (car old-nets)))))
      (setq old-nets (cdr old-nets)))
    (bbdb-record-set-net new-record new-nets))
  ;; addrs
  (let ((old-addresses (bbdb-record-addresses old-record))
    (new-addresses (bbdb-record-addresses new-record)))
    (while old-addresses
      (if (not (member (car old-addresses) new-addresses))
      (setq new-addresses (append new-addresses (list (car old-addresses)))))
      (setq old-addresses (cdr old-addresses)))
    (bbdb-record-set-addresses new-record new-addresses))
  ;; phones
  (let ((old-phones (bbdb-record-phones old-record))
    (new-phones (bbdb-record-phones new-record)))
    (while old-phones
      (if (not (member (car old-phones) new-phones))
      (setq new-phones (append new-phones (list (car old-phones)))))
      (setq old-phones (cdr old-phones)))
    (bbdb-record-set-phones new-record new-phones))
  ;; notes
  (let ((old-notes (bbdb-ensure-list (bbdb-record-raw-notes old-record)))
    (new-notes (bbdb-ensure-list (bbdb-record-raw-notes new-record))))
    (while old-notes
      (if (not (member (car old-notes) new-notes))
      (setq new-notes (append new-notes (list (car old-notes)))))
      (setq old-notes (cdr old-notes)))
    (bbdb-record-set-raw-notes new-record new-notes))
  ;; return
  new-record)

(defun bbdb-ensure-list (foo)
  (if (lisp foo) foo
    (list foo)
    )
  )

(defun bbdb-zulu (date)
  (if (fboundp 'bbdb-time-convert)
      (bbdb-time-convert date "%Y%m%d%H%Mz")
    date ;; bbdb1.51 does not use it anyway.
    )
)

(defun bbdb-unzulu (date)
  (if (eq (length date) 13)
      (format "%s-%s-%s" (substring date 0 4) (substring date 4 6) (substring date  6 8))
    date)
)
(defun bbdb-ldif-indent (str)
  (if (> (length str)  70)
      (concat (substring str 0 65) "\n " (bbdb-ldif-indent (substring str 65)))
    str)
)

(defun addnote (nrec nname note)
  (bbdb-record-set-raw-notes
   nrec (cons (cons nname note)  (bbdb-record-raw-notes nrec)  )
   )
  )

(defmacro alias-update ()
  (if (fboundp 'bbdb-define-all-aliases) (list 'bbdb-define-all-aliases))
)

(defmacro alias-setup ()
  (if (fboundp 'mail-aliases-setup) (list 'mail-aliases-setup))
)

(defmacro domailaliases ()
  (fboundp 'mail-aliases-setup)
)


(defmacro dodenote (st)
  (if (fboundp 'base64-decode-string)
      (list 'base64-decode-string  st)
    "?"
    )
)

(defun addtonote (ton str)
  (cond
   ((and ton str) (concat ton "\n" str))
   (str (concat "--bbdb--\n" str))
   (ton)
   )
  )

(defun setaddr (nrec afun val)
  (if (not (bbdb-record-addresses nrec))
      (let ((addr(make-vector bbdb-address-length "")))
    (bbdb-record-set-addresses nrec (list addr))
    (bbdb-address-set-location addr "address")
    )
    )
  (eval (list afun (car (bbdb-record-addresses nrec)) val))
  )


(defun setphone (nrec iloc pno np)
  (let ((nov (bbdb-parse-phone-number pno))
    (pv (make-vector bbdb-phone-length ""))
    (ploc iloc)
    )
    (if (and np (equal  (car np) (concat bbdb-ldif-prefixh "PhoneLoc")))
    (setq ploc (cdr np))
      )

  (if (and nov bbdb-north-american-phone-numbers-p)
      (progn
    (bbdb-phone-set-location pv ploc)
    (bbdb-phone-set-area pv (nth 0 nov))
    (bbdb-phone-set-exchange pv (nth 1 nov))
    (bbdb-phone-set-suffix pv (nth 2 nov))
    (bbdb-phone-set-extension pv (or (nth 3 nov) 0))
    )
    (setq pv (vector ploc pno))
    )
  (bbdb-record-set-phones nrec(append (bbdb-record-phones nrec)(list pv)))
  )
  )

(defun bbdb-string-fetch (key mls)
  (let ((tmls  (car mls)) res)
    (while (and (not res) (car tmls))
      (if (string-match (format "%s= *\\(.+\\)" key) (car tmls))
      (setq res (match-string 1 (car tmls))))
      (setq tmls (cdr tmls)))
    res
    )
  )

(defun bbdb-ldif-get-phone (atts df)
  (if (and (cdr atts) (equal (concat bbdb-ldif-prefixh "phoneloc") (caadr atts)))
      (cdr (cadr atts))
    df)
)

(defun bbdb-import-ldif ()
  "import LDIF entries for current buffer
Mailinglists \(groupOfNames\) are imported as entries in bbdb mail-alias fields."
  (interactive)
;    (message (concat  (/(* 100 (point)) (point-max)) " pct\n"))
;;    (message (concat "\nnew rec  at" (point)))
  (let ((reclist (split-string (buffer-substring 1 (point-max)) "\n[ \t\r]*\n"))
    (numr 0) maxr (opct 0) pct mailinglists (emptyrec (make-vector bbdb-record-length nil))
    )
    (setq maxr (length reclist))
    (mapcar
     (lambda (rec)
       (if (not (equal "" rec))
       (let (
         (atts (mapcar (lambda (at)
                 (if (equal (string-to-char at) ?\ )
                 (cons 'continuation (substring at 1))
                   (let ( (cpos  (string-match ":" at)))
                 (if cpos
                     (let ((cpos2 ( string-match "[^ \t]"  at (1+ cpos))))
                       (if cpos2
                       (cons (substring at 0 cpos) (substring at cpos2))
                     )
                       )
                   )
                 )
                   )
                 )
               (split-string  rec "[\n\r]+"))
           )
         )
     (setq pct (/ (* 100 numr) maxr))
     (if (/= opct pct)
         (progn
           (setq opct pct)
           (message (concat  pct " pct"))
           )
       )
     (setq numr (1+ numr))

     (if (member '("objectclass" . "groupOfNames") atts)
         (let (mlcn lmlist)
           (while atts
         (if (car atts)
             (let ((attName (downcase (caar atts)))
               (attVal (cdar atts))
               )
               (while (and (cdr atts) (equal (caadr atts) 'continuation))
             (setq atts (cdr atts))
             (setq attVal (concat attVal (cdar atts)))
             )
               (if (equal (string-to-char  attVal)  ?:)
                   (setq attVal (dodenote (substring attVal (string-match "[^: \t]" attVal)))))

               (cond
            ((or (equal attName "cn") (equal attName "commonname")) (setq mlcn attVal))
            ((equal attName "member")
             (setq lmlist (cons  (bbdb-split attVal ",") lmlist))
             )
            )
               )
           )
         (setq atts (cdr atts))
         ) ; while
           (setq mailinglists (cons (cons mlcn lmlist) mailinglists))
           )
       (let (
         (new-record   (make-vector bbdb-record-length nil)))
         (while  atts
           (if (stringp (car-safe (car-safe atts)))
         (let (
               (attName (downcase (caar atts)))
               (attVal (cdar atts))
               (nextAtt  (car-safe (cdr-safe atts)))
               )

           (while (and (cdr atts) (equal (caadr atts) 'continuation))
             (setq atts (cdr atts))
             (setq attVal (concat attVal (cdar atts)))
             )
           (if (equal (string-to-char  attVal)  ?:)
               (setq attVal
                 (dodenote (substring attVal (string-match "[^: \t]" attVal))))
               )
         (cond
          ;((or (equal attName "cn") (equal attName "commonname")) hmm)
          ((or (equal attName "sn") (equal attName "surname")) (bbdb-record-set-lastname new-record attVal))
          ((equal attName "givenname") (bbdb-record-set-firstname new-record attVal))
          ((equal attName "o") (bbdb-record-set-company new-record attVal))
          ((equal attName "locality") (setaddr new-record 'bbdb-address-set-city  attVal))
          ((equal attName "postalcode") (setaddr new-record 'bbdb-address-set-zip attVal))
          ((equal attName "st") (setaddr new-record 'bbdb-address-set-state  attVal))
          ((equal attName (concat bbdb-ldif-prefixh "mainaddrloc"))
           (setaddr new-record 'bbdb-address-set-location attVal))

          ;; This is ugly. But is it the only way Netscape understands.
          ((equal attName "postofficebox") (setaddr new-record 'bbdb-address-set-street1 attVal))
          ((equal attName "streetaddress") (setaddr new-record 'bbdb-address-set-street2  attVal))

          ((equal attName "mail")
           (bbdb-record-set-net new-record (cons attVal (bbdb-record-net new-record))))

          ((equal attName "mailalternateaddress")
           (bbdb-record-set-net new-record (append  (bbdb-record-net  new-record)
                                (list attVal)))
           )

          ((equal attName "postaladdress")
           (let (
             (alines (split-string (concat (bbdb-ldif-renl attVal) "\n")"[\n\r]"))
             (addr (make-vector bbdb-address-length "")))
             (if (and (string-match "^bbdb=" (nth 0 alines ))
                  (> (length alines) 6))
             (progn
               (bbdb-address-set-location addr (substring (nth 0 alines) 5))
               (bbdb-address-set-street1 addr (nth 1 alines))
               (bbdb-address-set-street2 addr (nth 2 alines))
               (bbdb-address-set-street3 addr (nth 3 alines))
               (bbdb-address-set-zip addr (nth 4 alines))
               (bbdb-address-set-city addr (nth 5 alines))
               (bbdb-address-set-state addr (nth 6 alines))
               (bbdb-record-set-addresses
                new-record
                (append (bbdb-record-addresses new-record) (list addr))
                )
               )
               )
             )
           )


          ((equal attName "homephone")
           (setphone new-record (bbdb-ldif-get-phone atts "Private") attVal nextAtt) )
          ((equal attName "facsimiletelephonenumber")
           (setphone new-record (bbdb-ldif-get-phone atts "Fax") attVal nextAtt))
          ((equal attName "pagerphone")
           (setphone new-record (bbdb-ldif-get-phone atts "pagerphone") attVal nextAtt))
          ((equal attName "cellphone")
           (setphone new-record (bbdb-ldif-get-phone atts "cellphone") attVal nextAtt))
          ((equal attName "mobiletelephonenumber")
           (setphone new-record (bbdb-ldif-get-phone atts "cellphone") attVal nextAtt))
          ((equal attName "telephonenumber")
           (setphone new-record (bbdb-ldif-get-phone atts "Work") attVal nextAtt))
          ((equal attName "xmozillanickname") (bbdb-record-set-aka  new-record (list attVal)))
          ((or (equal attName "description") (equal attName "multilinedescription"))
           (if (equal attName "multilinedescription")
               (setq attVal (bbdb-ldif-renl attVal)))
           (let ((thenote (substring attVal 0  (string-match "\n?--bbdb--\n" attVal))))
             (if (not (equal "" thenote))
             (addnote new-record 'notes  thenote)
             )
           )
           )

          ((equal attName "createTimestamp")
           (addnote new-record 'creation-date (bbdb-unzulu attVal)))
          ((equal attName "modifyTimestamp")
           (addnote new-record 'timestamp (bbdb-unzulu attVal)))
          ((eq  (string-match bbdb-ldif-prefix attName) 0)
           (let (
             (bbdb-ldif-note (make-symbol (substring attName (length bbdb-ldif-prefix)))))
             (bbdb-record-set-raw-notes new-record
                        (cons (cons bbdb-ldif-note attVal)
                              (bbdb-record-raw-notes new-record)))
             )
           )
          )
         )
         )
           (setq atts (cdr atts))
           )
       ;  (print new-record)
         (if (not (equal new-record emptyrec))
         (progn
           (bbdb-record-set-cache new-record (make-vector bbdb-cache-length nil))
           (let      ((old-record
;;               (and (bbdb-record-net new-record)
                  (bbdb-search-simple (tnsnil (bbdb-record-name new-record))
                              (car (bbdb-record-net new-record)))
;;                )
                 )
                  )
           (if old-record
               (progn
             (setq new-record (bbdb-merge-internally-ldif old-record new-record))
             (bbdb-delete-record-internal old-record)))
           ;; create  new record
           (bbdb-invoke-hook 'bbdb-create-hook new-record)
           (bbdb-change-record new-record t)
           (bbdb-hash-record new-record)
           )
           )
           )
         )
       )

     )
     ) ; if
     ) ; lambda
     reclist
     )
    (mapcar
     (lambda (mlist)
       (let (
         (mlcn (car mlist)) (lmlist (cdr mlist)))
     (if mlcn
         (while lmlist
           (let (
             (mnet (bbdb-string-fetch"mail"   lmlist))
             (mname (bbdb-string-fetch"cn" lmlist))
             (mcomp (bbdb-string-fetch"o"  lmlist))
;;           (mou (bbdb-string-fetch"ou" lmlist))
             (therecs (bbdb-records))
             therec
             mal
             )
          (if mnet (setq therecs (bbdb-search therecs nil nil mnet nil)))
         (if mname (setq therecs (bbdb-search therecs mname nil nil nil )))
         (if mcomp (setq therecs (bbdb-search therecs nil mcomp nil nil nil )))

         (cond ((not therecs)
            (message (concat "Mailing list member not found: " mname " " mnet)))
               ((= (length therecs) 1)
            (setq therec (car therecs))
            (setq mal  (assq 'mail-alias (bbdb-record-raw-notes therec)))
            (if (not mal)
                (progn
                  (setq mal (cons 'mail-alias ""))
                  (bbdb-record-set-raw-notes therec (cons mal (bbdb-record-raw-notes therec))))
              (bbdb-change-record therec nil)
              (bbdb-hash-record therec)
              )
            (if (not (member mlcn (split-string (cdr mal) "[, ]")))
                (setcdr mal (concat mlcn (if (>  (length  (cdr-safe mal)) 0) "," "") (cdr mal) )))
            )
         (t  (message "Mailing List member not unique %s, %s"  mname mnet))
         )
         )
           (setq lmlist (cdr lmlist))
           )
                    ;          (define-mail-alias cn lmlist)
       )
     )
       )
     mailinglists
     )
    )
(message nil)
)



(defun rmspace (str)
  (apply 'concat (bbdb-split str "\n\r")))

(defun bbdb-ldif-replace-string (str frs tos)
  (let ((start 0))
    (while (string-match frs str start)
      (setq str
        (concat (substring str 0 (match-beginning 0))
            tos
            (substring str (match-end 0))))
          (setq start (+  (length tos) (match-beginning 0))))
    )
str
)


(defun bbase64-encode-string (st)
  (concat ":" (bbdb-ldif-indent (rmspace  st))
      )
  )

(defun bbdb-ldif-rmnl (str)
  (bbdb-ldif-replace-string (bbdb-ldif-replace-string str "\\$" "\\24") "\n" "$")
)

(defun bbdb-ldif-renl (str)
  (bbdb-ldif-replace-string (bbdb-ldif-replace-string str "\\$" "\n") "\\\\24" "$")
)

(defmacro donote (st)
  (if (fboundp 'base64-encode-string)
      (list 'bbase64-encode-string (list 'base64-encode-string st))
    (list 'bbdb-ldif-rmnl st)
    )
)

(defun base64IfMulti (st)
  (if (string-match "\n" st)
      (donote st)
    (concat " " (bbdb-ldif-indent st))
  )
)

(defun nsloc (pl) "Guess mapping from userdefined bbdb locations to NS Work/Home/Fax"
  (let (
    (pld (and pl (downcase pl)))
    (fc (and pl (not (equal pl "")) (string-to-char (downcase pl))))
    )
    (cond  ( (not fc)   "telephonenumber")
        ((or (= fc ?a) (= fc ?w))  "telephonenumber")
       ( (= fc ?h)  "homephone")
;;     ( (= fc ?m)  "mobileTelephoneNumber")
       ( (equal pld "private")  "homephone")
       ( (= fc ?m)  "cellphone")
       ( (and (= fc ?p) (> (length pld) 1) (= (aref  pld 1) ?a)) "pagerphone")
       ( (equal pld "fax")  "facsimiletelephonenumber")
       ( t  "telephonenumber")
       )
    )
)

(defun tnil(tt)
  (if tt tt "?"))

(defvar ldifbuffer "*LDIF*" "Name of buffer for LDIF output")

(defun bbdb-to-ldif (visible-records) "Converts BBDB to LDIF format. Can be used to export bbdb to Netscape
Communicator Address book.\\<bbdb-mode-map>
If \"\\[bbdb-apply-next-command-to-all-records]\\[bbdb2ldif]\" is \
used instead of simply \"\\[bbdb2ldif]\", then includes only the
people currently in the *BBDB* buffer.
The result is placed in a buffer name \"*LDIF*\"
If  MEL is installed Multiline notes/descriptions work with Netscape address book.
Mail-aliases from mailrc file or bbdb mail-aliases fields are exported as mainglists
\(GroupOfNames\)
"
  (interactive (list
        (bbdb-do-all-records-p)
        )
           )
  (let* (
     (target (cons bbdb-define-all-aliases-field "."))
     (ldif-records
      (bbdb-search
       (if (not visible-records)
           (bbdb-records)
         (mapcar 'car bbdb-records)
         )
       nil nil nil target)
      )
     tmps
     record
     )


    (setq ldif-records
      (if (not visible-records)
          (bbdb-records)
        (mapcar 'car bbdb-records)
        )
      )

    (set-buffer (get-buffer-create ldifbuffer))
    (setq fill-column 1000)
    (erase-buffer)

    (while ldif-records
      (setq record (car ldif-records))
      (insert "\nxmozillausehtmlmail: FALSE\n")
      (let (
        (net (car (bbdb-record-net record)))
        (rnet  (bbdb-record-net record))
        )
    (insert (format "dn: cn=%s"  (tnil (bbdb-record-name record))))
    (if net
        (insert (format ",mail=%s" net))
        )
    (insert "\n")

    (setq tmps (bbdb-record-firstname record)) (insert "givenname: "  (tnil tmps) "\n")
    (setq tmps (bbdb-record-lastname record))   (if tmps (insert "sn: "  tmps "\n"))
    (insert "objectclass: top\nobjectclass: person\n")
    (setq tmps (bbdb-record-company record))    (if tmps (insert "o: " tmps "\n"))
    (setq tmps (bbdb-record-name record))   (if tmps (insert "cn: "  tmps "\n"))

    (if net (insert "mail: " net "\n"))
    (while (cdr rnet)
      (insert "mailAlternateAddress: " (cadr rnet) "\n")
      (setq rnet (cdr rnet))
      )
    )
      (let (
        (phones (bbdb-record-phones record))
        (addrs (bbdb-record-addresses record))
        (aka (bbdb-record-aka record))
        (firstaddr t)
        tonote
        phone
        (elide nil)
        )

    (while phones
      (setq phone (car phones))
      (if (equal (nsloc (bbdb-phone-location phone))"cellphone")
          (setq tonote (addtonote tonote (concat "M:" (bbdb-phone-string phone) )))
        )
      (if (equal (nsloc (bbdb-phone-location phone))"pagerphone")
          (setq tonote (addtonote tonote (concat "P:" (bbdb-phone-string phone) )))
        )
      (insert (format "%s: " (nsloc (bbdb-phone-location phone))) (bbdb-phone-string phone) "\n")
      (insert bbdb-ldif-prefixh "PhoneLoc:" (bbdb-phone-location  phone)"\n")
      (setq phones (cdr phones)))

    (let (addr tmps)
      (while  addrs
        (setq addr (car addrs))
        (if firstaddr (progn
        (if (= 0 (length (setq tmps (bbdb-address-street1 addr)))) nil  (insert "postOfficeBox: " tmps "\n"))
        (if (= 0 (length (setq tmps (bbdb-address-street2 addr)))) nil  (insert "streetaddress: " tmps "\n"))
        (if (= 0 (length (setq tmps (bbdb-address-street3 addr)))) nil  (insert "streetaddress: " tmps "\n" ))

       ; This does not work with Netscape
       ; (if (= 0 (length (setq tmps (bbdb-address-street1 addr)))) nil  (insert "homePostalAddress:" tmps ))
       ; (if (= 0 (length (setq tmps (bbdb-address-street2 addr)))) nil  (insert "$" tmps))
       ; (if (= 0 (length (setq tmps (bbdb-address-street3 addr)))) nil  (insert "$" tmps ))
       ; (insert "\n")

        (insert "locality:"  (bbdb-address-city addr) "\n")
        (setq tmps (bbdb-address-state addr))
        (if (and tmps (not (equal tmps ""))) (insert "st:" tmps "\n"))
        (if (bbdb-address-zip-string addr)
            (insert "postalcode:" (bbdb-address-zip-string addr) "\n"))
        (setq firstaddr nil)
        )
          (progn
        (setq tonote (addtonote tonote (concat (bbdb-address-street1 addr))))
        (setq tonote (addtonote tonote (concat (bbdb-address-street2 addr))))
        (setq tonote (addtonote tonote (concat (bbdb-address-street3 addr))))
        (setq tonote (addtonote tonote (concat  (bbdb-address-zip-string addr) " "  (bbdb-address-city addr) )))
        (insert (concat "postalAddress: "
                (base64IfMulti (concat "bbdb=" (bbdb-address-location addr)  "\n"
                               (bbdb-address-street1 addr)  "\n"
                               (bbdb-address-street2 addr)  "\n"
                               (bbdb-address-street3 addr) "\n"
                               (bbdb-address-zip-string addr) "\n"
                               (bbdb-address-city addr) "\n"
                               (bbdb-address-state addr)
                               )
                           )
                "\n"
                )
            )
        )
          )
        (setq addrs (cdr addrs)))
      )
    (cond (aka
           (insert (format "%s: %s\n" "xmozillanickname"
                   (mapconcat (function identity) aka ", ")))
           ))
    (let ((notes (bbdb-record-raw-notes record)))
      (if (stringp notes)
          (setq notes (list (cons 'notes notes))))
      (while notes
        (setq elide nil)
        (cond
         ((member (caar notes) bbdb-elided-export-ldif) (setq elide t))
         ((eq (car (car notes)) 'creation-date)
          (insert "createTimestamp: " (bbdb-zulu (cdar notes))"\n")
          (setq elide t)
          )
         ((eq (car (car notes)) 'timestamp)
          (setq elide t)
          (insert "modifyTimestamp: "(bbdb-zulu (cdar notes))"\n")
          )
         ((eq (car (car notes)) 'notes)  (setq elide t))
         ((eq (car (car notes)) 'mail-alias)  (setq elide t))
         (t
        ;; Netscape cannot display this. So we also put it in the notes field.
        (setq tonote (addtonote tonote (format "%s:%s" (caar notes)   (cdar notes))))
        (insert (format "%s%s:" bbdb-ldif-prefix (car (car notes))))
         )
          )
        (if (eq (caar notes) 'notes)
        (if tonote
            (setq tonote (concat (cdar notes) "\n" tonote))
          (setq tonote  (cdar notes)))
          (if (not elide)
          (insert (base64IfMulti (tnil (cdar notes))) "\n"))
          )
        (setq notes (cdr notes))
        )
      (if tonote
          (if (and (string-match "\n" tonote) (not (fboundp 'base64-encode-string)))
          (insert "multilineDescription:" (bbdb-ldif-rmnl tonote ) "\n")
          (insert "description:" (base64IfMulti tonote ) "\n")
          )
        )
      )
    (if (bbdb-record-addresses record)
        (insert bbdb-ldif-prefixh "mainAddrLoc:" (bbdb-address-location (car (bbdb-record-addresses record)))"\n")
        )

    )
      (setq ldif-records (cdr ldif-records))
      )
    )
  (if (and (not visible-records) (domailaliases))
      (progn
    (alias-update)
    (alias-setup)
    ;;      (bbdb-define-all-aliases)
    (let ((mai 0) mae alist (malen (length mail-aliases)
                     ))
    (while (< mai malen)
      (setq mae (aref mail-aliases mai) )
      (if (and mae (symbolp mae ))
          (progn
        (insert (format "\ndn: cn=%s\n"  mae))
        (insert (format "cn: %s\n"  mae))
        (insert "objectclass: top\n")
        (insert "objectclass: groupOfNames\n")
        (setq alist (symbol-value mae ))
        (if alist
             (mapcar
              (lambda (an)
            (let ((trec (bbdb-search-simple nil an))
                  )
              (if trec
              (insert (format "member: cn=%s,mail=%s\n"
                      (tnil (bbdb-record-name trec))
                      (tnil (car (bbdb-record-net trec)))
                      )
                  )
              )
              )
            )
              (split-string alist ", ")
              )
             )
        )
        )
      (setq mai (1+ mai))
      )
    )
    )
    (alias-update)
    )
  (set-window-buffer (get-lru-window) ldifbuffer )
)
;;(add-hook 'bbdb-load-hook (lambda () (define-key bbdb-mode-map "L"      'bbdb-to-ldif)))
(define-key bbdb-mode-map "L"      'bbdb-to-ldif)
(provide 'bbdb-ldif)
