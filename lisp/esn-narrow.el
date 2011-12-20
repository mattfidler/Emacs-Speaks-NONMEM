;;; esn-narrow.el --- Narrowing and current record routines
;; 
;; Filename: esn-narrow.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Aug 11 13:40:39 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 27 18:59:20 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 223
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 20-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Jan 20 10:17:21 2011 (-0600) #220 (Matthew L. Fidler)
;;    added `esn-point-at-bor'
;; 11-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 11 11:27:53 2010 (-0600) #206 (Matthew L. Fidler)
;;    Made sure that esn-current-abbrev-records-regexp is specified.
;; 11-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 11 10:07:35 2010 (-0600) #198 (Matthew L. Fidler)
;;    Changed esn-get-current rec to return nothing when there are no prior records.
;; 20-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 20 12:37:59 2010 (-0500) #186 (Matthew L. Fidler)
;;    Made esn-is-abbreviated to return 't everywhere in a record.
;; 20-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Aug 20 11:46:55 2010 (-0500) #172 (Matthew L. Fidler)
;;    Added bug-fix to esn-narrow-record.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 09:21:03 2010 (-0500) #150 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 08:53:24 2010 (-0500) #147 (Matthew L. Fidler)
;;    Moved esn-narrow-to-current-record here, moved out esn-rec3.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 08:49:54 2010 (-0500) #144 (Matthew L. Fidler)
;;    Added a force option for esn-get-current-record.
;; 16-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 16 10:50:31 2010 (-0500) #133 (Matthew L. Fidler)
;;    Added a method to distinguish $INFILE and $INFN
;; 11-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 11 13:40:52 2010 (-0500) #1 (Matthew L. Fidler)
;;    Changed to use cookies
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

;;;###autoload
(defun esn-narrow-to-current-problem ()
  (interactive)
  (let (
	(pb nil)
	(pe nil))
    (save-excursion
      (if (re-search-backward "\\<$PRO" nil t)
	  (setq pb (point))
	(setq pb (point-min)))
      (end-of-line)
      (if (re-search-forward "\\<\\$PRO" nil t)
	  (progn
	    (beginning-of-line)
	    (setq pe (point)))
	(setq pe (point-max)))
      (narrow-to-region pb pe))))

(defvar esn-overlay-category-abbrev-rec nil
  "Abbreviated Record Overlay")
(defvar esn-overlay-category-rec nil
  "Record Overlay")

(defvar esn-overlay-category-problem nil
  "Problem Overlay")


(defalias 'esn-get-current-record 'esn-get-current-rec)
(defalias 'esn-current-rec 'esn-get-current-rec)
;;;###autoload
(defun esn-point-at-bor ()
  "Point at beginning of record"
  (esn-get-current-rec)
  esn-get-current-record-start)
;;;###autoload
(defun esn-last-record-same-p (&optional force-recalc)
  "Returns true if the last record was the same as the current record."
  (esn-get-current-rec force-recalc)
  (and esn-get-current-record-start
       esn-get-current-record-stop
       esn-get-current-record-stop2
           esn-get-current-record-val
           esn-get-current-record-len
           esn-get-current-record-eof
           (= (point-max) esn-get-current-record-eof)
           (>= (point) esn-get-current-record-start)
           (<= (point) esn-get-current-record-stop)))
;;;###autoload
(defun esn-get-current-rec (&optional force-recalc)
  "Returns the current record, otherwise returns \"\".

Current Record = Three letters of current record excluding $

For $AES0 and $AESINITIAL return AES0

"
  (interactive)
  (let ( 
        (case-fold-search 't)
        (ret "")
        begin
        oend
        end
        len
        tmp
        (rec-reg (esn-reg-records 't))
        (pt (point)))
    (if (and (not force-recalc) esn-get-current-record-start
             esn-get-current-record-stop
             esn-get-current-record-stop2
             esn-get-current-record-val
             esn-get-current-record-len
             esn-get-current-record-eof
             (= (point-max) esn-get-current-record-eof)
             (>= (point) esn-get-current-record-start)
             (cond
               ( (<= (point) esn-get-current-record-stop)
                 (setq ret esn-get-current-record-val)
                 't)
               ( (<= (point) esn-get-current-record-stop2)
                 (setq ret "")
                 't)
               ( 't
                 nil)))
        (symbol-value 'ret)
      (save-excursion
        (save-match-data
          (if (not (re-search-backward rec-reg nil t))
              (progn
                (setq ret "")
                ;; need to set begin and end and oend
                (setq begin (point-min))
                (setq end begin)
                (if (not (re-search-forward rec-reg nil t))
                    (progn ;; No records found.  Begin=end=beginning of buffer; oend=end of buffer
                      (setq oend (point-max)))
                  ;; Found another record set oend to the beginning of that record.
                  (setq oend (match-beginning 0))))
            ;; Check for next 
            (setq ret (match-string 0))
            (setq len (length (match-string 0)))
            (setq begin (point))
            (forward-char len)
            (if (not (re-search-forward rec-reg nil t))
                (setq end (point-max))
              (backward-char (length (match-string 0)))
              (looking-at ".*")
              (setq end (point)))
            (save-restriction
              (narrow-to-region begin end)
              (goto-char (point-max))
              (skip-chars-backward " \t\n")
              (while (re-search-backward ";.*\\=" nil t)
                (skip-chars-backward " \t\n"))
              ;; Add back wrapped and double comments.
              (when (save-excursion 
                      (beginning-of-line)
                      (re-search-forward "^\\=\\(.+\\);.*$" nil t))
                (setq tmp (length (match-string 1)))
                (re-search-forward "\\=\n" nil t)
                (while (re-search-forward (format "^ \\{%s\\};.*$" tmp) nil t)
                  (re-search-forward "\\=\n" nil t)))
              (end-of-line))
            (setq oend end)
            (setq end (point)))))
      (if (and begin end 
               (or 
                (< (point) begin)
                (> (point) end)))
          (setq ret "")
        (setq ret (esn-rec3 ret))
        (unless ret
          (setq ret ""))
        (setq esn-get-current-record-start begin)
        (setq esn-get-current-record-stop end)
        (setq esn-get-current-record-stop2 oend)
        (setq esn-get-current-record-val ret)
        (setq esn-get-current-record-eof (point-max))
        (setq esn-get-current-record-len len)
        (unless esn-current-abbrev-records-regexp
          (esn-switch-variables)
          (unless esn-current-abbrev-records-regexp
            (message "Warning: `esn-current-abbrev-records-regexp' is undefined.  Somethings wrong with either the cookies OR `esn-switch-variables'")))
        (when esn-current-abbrev-records-regexp
          (setq esn-get-current-record-abbrev (string-match esn-current-abbrev-records-regexp (concat "$" ret)))))
      (symbol-value 'ret))))

;;;###autoload
(defalias 'esn-narrow-rec 'esn-narrow-record
  "* Narrows record")

;;;###autoload
(defun esn-is-abbrev-p (&optional only-rec )
  "* Is the current record an abbreviated record?"
  (if only-rec
      (and (esn-get-current-rec) esn-get-current-record-abbrev)
    (esn-get-current-rec)
    (and esn-get-current-record-abbrev
         esn-get-current-record-start
         esn-get-current-record-stop2
         (>= (point) esn-get-current-record-start)
         (<= (point) esn-get-current-record-stop2))))

;;;###autoload
(defun esn-narrow-record (&optional force-recalculate)
  "This function narrows the buffer to the current record.
   If outside of the record, esn-narrow-rec does nothing.
Returns the indentation length"
  (interactive)
  (let (
        (ret nil))
    (esn-get-current-rec force-recalculate)
    (when (and 
           esn-get-current-record-start
           esn-get-current-record-stop
           esn-get-current-record-stop2
           esn-get-current-record-val
           esn-get-current-record-len
           esn-get-current-record-eof
           (= (point-max) esn-get-current-record-eof)
           (>= (point) esn-get-current-record-start)
           (<= (point) esn-get-current-record-stop))
      (narrow-to-region esn-get-current-record-start
                        esn-get-current-record-stop)
      (setq ret (+ 1 esn-get-current-record-len)))
    (symbol-value 'ret)))
(provide 'esn-narrow)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-narrow.el ends here
