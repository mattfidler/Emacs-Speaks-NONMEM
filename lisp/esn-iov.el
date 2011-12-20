;;; esn-iov.el --- IOV functions for Emacs Speaks NONMEM
;; 
;; Filename: esn-iov.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Oct  6 10:19:09 2010 (-0500)
;; Version: 
;; Last-Updated: Thu Sep  1 15:36:04 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 107
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `esn-start'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 17-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Mon Jan 17 12:15:05 2011 (-0600) #94 (Matthew L. Fidler)
;;    Made esn-occ require esnR package.
;; 06-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct  6 12:14:05 2010 (-0500) #62 (Matthew L. Fidler)
;;    Initial release.  Should let IOV# be completed.
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(declare-function esn-R-input-data "esn-ess")
(declare-function esn-alert "esn-exec")
(declare-function esn-R-load-esnR "esn-ess")
(declare-function esn-R-list "esn-ess")
(declare-function esn-max-eta "esn-properties")

(require 'esn-start)

;;;###autoload
(defun esn-occ ()
  "Puts Occasion in $PK or $PRED block if not present"
  (save-match-data
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (unless (re-search-forward "\\<OC[0-9]+\\>" nil t)
          (when (re-search-forward (eval-when-compile (esn-reg-record-exp '("PK" "PRED") t)))
            (insert "\n")
            (insert (esn-get-occ))))))))

(defun esn-get-occ ()
  "Gets Occasion.  Requires Emacs Speaks Statistics interface.  Also requires data item to be labeled OCC."
  (if (not (esn-R-input-data))
      (esn-alert "EsN can generate OCC variable, but needs access to R through Emacs Speaks Statistics.")
    (esn-R-load-esnR)
    (let (
          (occ (esn-R-list (concat "sort(unique(esn.input(\""
                                   (buffer-file-name)
                                   "\")$OCC))")))
          (ret ""))
      (setq ret (mapconcat
                 (lambda(oc)
                   (format "  OC%s = 0\n  IF (OCC.EQ.%s) OC%s = 1" oc oc oc))
                 occ
                 "\n"))
      (symbol-value 'ret))))

(defun esn-iov-get-occ ()
  "* Gets all the occasion variables in the control stream.  Assumes occasion variables are form by:
OC#
"
  (let (
        (occ '())
        )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward esn-iov-occ nil t)
        (add-to-list 'occ (match-string 0))
        )
      (setq occ (sort occ
                      (lambda(a1 a2)
                        (let (num1 num2 ret)
                          (when (string-match "[0-9]+$" a1)
                            (setq num1 (string-to-number (match-string 0 a1)))
                            (when (string-match "[0-9]+$" a2)
                              (setq num2 (string-to-number (match-string 0 a2)))
                              (setq ret (< num1 num2))
                              )
                            )
                          (symbol-value 'ret)
                          )
                        )
                      )
            )
      (symbol-value 'occ)
      )
    )
  )
(defun esn-iov-number (number ret2-q)
  "* Inserts text for IOV number x, and creates corresponding $OMEGA blocks to be inserted by Yasnippet if available."
  (let (
        (occ (esn-iov-get-occ))
        (i 0)
        (eta (+ 1 (esn-max-eta 't)))
        ret
        (ome "$OMEGA BLOCK(1) $1 ; IOV%s %s")
        )
    (setq ret (mapconcat
               (lambda(x)
                 (let (r)
                   (setq r (format "%s%s*ETA(%s)" (if (= 0 (mod i 3)) "!" "") x eta))
                   (set ret2-q (concat (symbol-value ret2-q) "\n" (format ome number x)))
                   (setq ome "$OMEGA BLOCK(1) SAME ; IOV%s %s")
                   (setq i (+ i 1))
                   (setq eta (+ eta 1))
                   
                   (symbol-value 'r)
                   )
                 )
               occ
               "+"
               )
          )
    (set ret2-q (concat (symbol-value ret2-q) "\n"))
    (while (string-match (regexp-quote "+!") ret)
      (setq ret (replace-match (format "\n  IOV%s = IOV%s+" number number) 't 't ret))
      )
    (while (string-match (regexp-quote "!") ret)
      (setq ret (replace-match "" 't 't ret)))
    (symbol-value 'ret)
    )
  )
;;;###autoload
(defun esn-iov ()
  "* Creates IOV definition"
  (interactive)
  (let (
        has-equals
        (ret2 "")
        number
        (debug-on-error 't)
        )
    (when (looking-back "^[ \t]*IOV\\([0-9]+\\)[ \t=]*" nil t)
      (esn-occ)
      (setq number (match-string 1))
      (setq has-equals (string-match "=" (match-string 0)))
      (unless has-equals
        (insert "= ")
        )
      (insert (esn-iov-number number 'ret2))
      (setq esn-yas-pre-point (point))
      (esn-insert-after ret2 (append esn-yas-insert-omega-after (list "OME")) 't 't))))

;;;###autoload
(defun esn-iov2 ()
  "Creates an IOV definition outside of current location"
  (interactive)
  (when (looking-back "\\<IOV\\([0-9]+\\)")
    (let ((iov (match-string 1)))
      (unless (save-excursion (re-search-backward (concat "\^[ \t]*IOV" iov "[ \t]*=") nil t))
        (when (re-search-backward (eval-when-compile (esn-reg-record-exp '("PK" "PRE") nil t)) nil t)
          (goto-char (match-end 0))
          (while (re-search-forward "\\<OC[0-9]+[ \t]*=" nil t)
            (end-of-line))
          (insert "\n  IOV" iov "=")
          (esn-iov))))))


(provide 'esn-iov)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-iov.el ends here
