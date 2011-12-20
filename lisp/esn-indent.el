;;; esn-indent.el --- Indentation routines.
;; 
;; Filename: esn-indent.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 16:18:00 2010 (-0500)
;; Version: 
;; Last-Updated: Tue Sep  6 20:40:34 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 105
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
;; 06-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Dec  6 14:43:23 2010 (-0600) #94 (Matthew L. Fidler)
;;    Bug fix for indentation function...
;; 15-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Nov 15 14:52:03 2010 (-0600) #89 (Matthew L. Fidler)
;;    Bug fix for comment indentation. 
;; 20-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 20 13:05:27 2010 (-0500) #78 (Matthew L. Fidler)
;;    Bugfix for abbreviated records indentation.
;; 17-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 17 15:15:02 2010 (-0500) #57 (Matthew L. Fidler)
;;    Rewrote indentation routines.
;; 07-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep  7 13:32:04 2010 (-0500) #6 (Matthew L. Fidler)
;;    Removed esn-not-indented-to-length and changed to abbreviated records.
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

(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-is-abbrev-p "esn-narrow")

(require 'esn-start)
;;;###autoload
(defun esn-indent-line ()
  "* Indents a line in EsN"
  (interactive)
  (when esn-mode-auto-indent
    (let (
          (case-fold-search 't)
          (curi 0)
          (deindent nil)
          p1
          )
      (unless (save-excursion ;; indentation 0 for records & comments starting at the zero column.
                (goto-char (point-at-bol))
                (looking-at "\\([ \t]*[$]\\|;\\)")
                )
        (cond 
         ( (string-match (esn-reg-record-exp esn-mode-auto-indent-force-zero-indent) (esn-get-current-rec))
           (setq curi 0)
           )
         ( (save-excursion (skip-chars-backward " \t\n") (esn-is-abbrev-p))
           ;; Abbreviated records
           (if (save-excursion
                 (goto-char (point-at-bol)) 
                 (skip-chars-forward " \t") 
                 (or 
                  (looking-at esn-mode-deindent-keyword-regexp)
                  (looking-at esn-mode-deindent-indent-keyword-regexp)
                  )
                 )
               (setq deindent 't)
             )
           (save-excursion
             (setq p1 (point-at-eol)) 
             (forward-line -1) 
             (if (= p1 (point-at-eol)) 
                 nil
               (goto-char (point-at-eol))
               (skip-chars-backward " \t\n")
               (setq curi (current-indentation))
               (goto-char (point-at-bol))
               (skip-chars-forward " \t")
               (cond
                ( deindent
                  ;; Deindent
                  (setq curi (max 2 (- curi 2))))
                ( (looking-at "[$]")
                  ;; First Record item
                  (setq curi 2))
                ( (looking-at esn-mode-indent-keyword-regexp)
                  ;; Indent
                  (setq curi (+ 2 curi)))
                ( (looking-at esn-mode-deindent-indent-keyword-regexp)
                  ;; Indent after a deindent
                  (setq curi (+ 2 curi)))
                ( 't
                                        ; Keep current indentation.
                  )))))
         ( 't 
           ;; All Others, do hanging indent.
           (save-excursion
             (forward-line -1)
             (end-of-line)
             (unless (string= "" (esn-get-current-rec))
               (when (re-search-backward (eval-when-compile (esn-reg-records)) nil 't)
                 (setq curi (+ 1 (length (match-string 0))))))))))
      (esn-indent-line-to curi)
      (when (looking-at "[ \t]*$")
        (goto-char (match-end 0))))))

;; Change log:
;; 19-Aug-2010    Matthew L. Fidler  
;;    Tried to standardize record regular expressions.

(defun esn-indent-line-to (w)
  "Insert spaces rather than tabs."
  (let (
        (case-fold-search 't)
        (what w))
    (if (< what 0)
        (setq what 0)
      )
    (save-excursion
      (beginning-of-line)
      (if (looking-at "^[ \t]*")
          (replace-match (make-string what ? ))
        )
      )
    )
  )
(provide 'esn-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-indent.el ends here
