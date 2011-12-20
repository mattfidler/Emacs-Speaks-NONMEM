;;;; esn-reg.el --- Global regular expressions for EsN
;; 
;; Filename: esn-reg.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Aug 19 09:18:15 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 27 18:58:46 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 174
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
;; 30-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov 30 11:36:37 2010 (-0600) #147 (Matthew L. Fidler)
;;    Changed data matching algorithm to only keep data match (exclude quotes)
;; 08-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep  8 17:17:16 2010 (-0500) #140 (Matthew L. Fidler)
;;    Bug fix for lists in esn-reg-record-exp
;; 08-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep  8 09:47:10 2010 (-0500) #135 (Matthew L. Fidler)

;;    Changed esn-reg-record-exp to accept rec as a regular expression with \\|
;;    and just return it if necessary.  This allows esn-rec to accept regular
;;    expressions instead of just a record or list of records.

;; 31-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Tue Aug 31 15:51:57 2010 (-0500) #117 (Matthew L. Fidler)
;;    Change esn-reg-record-exp to allow both strings and lists of records.
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

(defvar esn-reg-filename "\\(?:[^,;=() \t\n'\"]+\\|'[^'\n]+'\\|\"[^\"\n]+\"\\)"
  "* Regular expression for file-name")
(defvar esn-reg-filename-2 "\\([ \"']\\)\\(.+?\\)\\(?:\\1\\|$\\)"
  "* Regular expression for file-name")
(defvar esn-reg-filename-3 "\\(\\b\\)\\(.+?\\)\\b"
  "* Regular expression for file-name, form #3")
(defvar esn-reg-filename-partial "\\(?:[^,;=() \t\n'\"]*\\|'[^'\n]*\\|\"[^\"\n]*\\)"
  "* Regular expression for partial file-name"
  )
(defun esn-reg-records (&optional no-match skip-spaces-tabs-and-returns)
  "* Returns regular expression of records with/without a match"
  (concat 
   (if no-match
       "\\<\\(?:[$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)~*\\>"
     "\\<\\([$][A-Z][A-Z0-9_]*\\|INC[A-Z0-9_]*\\)~*\\>" ;
     )
   (if skip-spaces-tabs-and-returns
       "[ \t\n]*"
     "")))

(defun esn-reg-data-rec (&optional rec partial)
  "* Returns a regular expression for record rec (list or string) that has a
file-name associated with the second element of the match.  When partial is selected, this regular expression is only a partial record"
  (format "%s%s"
          (esn-reg-record-exp (or rec "DAT") 't)
          (if partial
              (concat "[ \t\n]*\\(" esn-reg-filename-partial "\\)")
            (concat "[ \t\n]*?" esn-reg-filename-2))))

(defun esn-reg-record-exp (rec &optional no-match skip-spaces-tabs-and-returns)
  "* Returns a regular expression for record rec."
  (save-match-data
    (if (and (eq (type-of rec) 'string)
             (string-match (eval-when-compile (regexp-quote "\\|")) rec))
        (symbol-value 'rec)
      (let (
            lst
            opt-lst
            ret
            )
        (if (eq (type-of rec) 'string)
            (setq lst (list rec))
          (setq lst rec)
          )
        (mapc
         (lambda(x)
           (cond
            ( (string= (esn-rec3 x) "DAT")
              ;; Look for data or infile
              (add-to-list 'opt-lst "$DAT#")
              (add-to-list 'opt-lst "$INFI#")
              )
            ( (string= (esn-rec3 x) "INF")
              (add-to-list 'opt-lst "$INFN#")
              (add-to-list 'opt-lst "$INF#")
              )
            ( (string= (esn-rec3 x) "AES0")
              (add-to-list 'opt-lst "$AES0#")
              (add-to-list 'opt-lst "$AESI#")
              )
            ( (string= (esn-rec3 x) "INC")
              (add-to-list 'opt-lst "$INC#")
              (add-to-list 'opt-lst "INC#")
              )
            ( (or (string= (esn-rec3 x) "BIN") (string= x "INP"))
              (add-to-list 'opt-lst (format "$%s#" (esn-rec3 x)))
              (add-to-list 'opt-lst (format "$%s!" (esn-rec3 x)))
              )
            ( 't
              (add-to-list 'opt-lst (format "$%s#" (esn-rec3 x)))
              )
            )
           )
         lst
         )
        (setq ret (format "\\<%s%s\\>"
                          (regexp-opt opt-lst (not no-match) )
                          (if skip-spaces-tabs-and-returns
                              "[ \t\n]*"
                            ""
                            )
                          )
              )
        (while (string-match (format "\\(%s\\|%s\\)" 
                                     (regexp-quote "[#!]")
                                     (regexp-quote "[!#]")
                                     ) ret)
          (setq ret (replace-match "[A-Z0-9_]*~*" nil nil ret)))
        (while (string-match "[#]" ret)
          (setq ret (replace-match "[A-Z0-9_]*" nil nil ret))
          )
        (while (string-match "!" ret)
          (setq ret (replace-match "[A-Z0-9_]*~*" nil nil ret)))
        (symbol-value 'ret)
        )
      )
    )
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generate Cookies Quote
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun esn-gen-cookies-quote (txt)
  (when txt
    (let (
          (ret txt)
          (case-fold-search nil)
          )
      (with-temp-buffer
        (insert ret)
        (goto-char (point-min))
        (while (re-search-forward (format "\\[[A-Z0-9_]*%s[A-Z0-9_]*\\]" (regexp-quote "[A-Z0-9_]*")) nil t)
          (replace-match "[A-Z0-9_]*" nil t)
          )
        (goto-char (point-min))
        (while (re-search-forward (eval-when-compile (format "\\[\\([A-Z0-9_]*\\)%s\\([A-Z0-9_]*\\)\\]" (regexp-quote (esn-reg-records 't)))) nil t)
          (replace-match "\\\\(?:INC[A-Z0-9_]*\\\\|[$][A-Z][A-Z0-9_]*\\\\|[\\1\\2]\\\\)" 't))
        (goto-char (point-min))
        (while (re-search-forward (format "\\[\\([A-Z0-9_]+\\)%s\\([A-Z0-9_]+\\)\\]\\([*+?]?\\)" (regexp-quote "[0-9]+")) nil t)
          (replace-match "\\\\(?:[0-9]+\\\\|[\\1\\2]\\3\\\\)" nil nil)
          )
        (goto-char (point-min))
        (while (re-search-forward (format "\\[%s\\([A-Z0-9_]+\\)\\]\\([*+?]?\\)" (regexp-quote "[0-9]+")) nil t)
          (replace-match "\\\\(?:[0-9]+\\\\|[\\1]\\2\\\\)" nil nil)
          )
        (goto-char (point-min))
        (while (re-search-forward (format "\\[\\([A-Z0-9_]+\\)%s\\]\\([*+?]?\\)" (regexp-quote "[0-9]+")) nil t)
          (replace-match "\\\\(?:[0-9]+\\\\|[\\1]\\2\\\\)" nil nil)
          )
        (goto-char (point-min))
        (while (search-forward "*?" nil t)
          (replace-match "*"))
        (goto-char (point-min))
        (while (re-search-forward "\\\\" nil 't)
          (replace-match "\\\\" 't 't))
        (goto-char (point-min))
        (while (re-search-forward "\"" nil 't)
          (replace-match "\\\"" 't 't))
        (goto-char (point-min))
        (while (re-search-forward "\n" nil 't)
          (replace-match "\\n" 't 't))
        (goto-char (point-min))
        (while (re-search-forward "\t" nil 't)
          (replace-match "\\t" 't 't))
        (goto-char (point-min))
        (setq ret (buffer-substring (point-min) (point-max)))
        )
      (symbol-value 'ret)
      ))
  )
(provide 'esn-reg)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-reg.el ends here
