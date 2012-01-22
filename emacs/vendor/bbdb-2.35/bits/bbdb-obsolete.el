;;; bbdb-obsolete-net.el -- Handle obsolete-net addresses.

;; Copyright (C) 2001 Colin Rafferty

;; Author: Colin Rafferty <colin@xemacs.org>
;; Keywords: bbdb, net, obsolete
;; Version: $Id: bbdb-obsolete.el,v 1.1 2002/06/29 23:59:19 waider Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to colin@xemacs.org) or from
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Send bug reports to colin@xemacs.org

;;; Commentary:

;; My ~/.bbdb is seven years old.  People change jobs, change ISPs,
;; and change nyms.  Their email addresses change.

;; While I no longer want to send someone email at an old address, I
;; still have old messages with the old addresses, and I want to match
;; up.

;; Move the old addresses into the field `obsolete-net', and:

;; (setq 'bbdb-obsolete-net-canonicalize-net-hook 'bbdb-canonicalize-net-hook)

;; If you already have a `bbdb-obsolete-net-canonicalize-net-hook',
;; then call `bbdb-canonicalize-net-hook' from within your function.

;;; Code:

(require 'bbdb-com)

(defgroup bbdb-obsolete-net nil
  "Customizations for setting up obsolete network addresses."
  :group 'bbdb)

(defcustom bbdb-obsolete-net-field 'obsolete-net
  "*Field in which to add the obsolete net addresses."
  :group 'bbdb-obsolete-net
  :type 'symbol)

;;;###autoload
(defun bbdb-obsolete-net-canonicalize-net-hook (addr)
  "Return user's current net address given obsolete ADDR.

Return ADDR if it is not obsolete anywhere, or there is no net address
in the matching record.  The field is set in `bbdb-obsolete-net-field'."
  (let* ((notes (cons bbdb-obsolete-net-field (concat "\\<" (regexp-quote addr) "\\>")))
         (records (bbdb-search (bbdb-records) nil nil nil notes)))
    (or (and (not (null records)) (car (bbdb-record-net (car records)))) addr)))

(provide 'bbdb-obsolete-net)

;;; bbdb-obsolete-net.el ends here
