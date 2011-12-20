;;; esn-fontlock.el --- New Font Lock Routines
;;
;; Filename: esn-fontlock.el
;; Description: New Font Lock Routines
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Fri Aug  6 08:42:29 2010 (-0500)
;; Version: 0.1
;; Last-Updated: Mon May  2 11:38:50 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 1691
;; URL: 
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This, along with version specific regular expressions, provides the
;;  font-locking mechanism of Emacs Speaks NONMEM
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 10-Feb-2011    Matthew L. Fidler  
;;    Last-Updated: Tue Jan 18 14:55:25 2011 (-0600) #1684 (Matthew L. Fidler)
;;    Changed Fontlock to byte-compile without being loaded.
;; 18-Jan-2011      
;;    Last-Updated: Tue Jan 18 14:52:50 2011 (-0600) #1680 (Matthew L. Fidler)
;;    Added italicizing of fixed variables as well as highlighting (in
;;    red) fixed durations
;; 11-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 11 07:59:40 2010 (-0600) #1563 (Matthew L. Fidler)
;;    Added linking operators
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 10:37:57 2010 (-0500) #1536 (Matthew L. Fidler)
;;    Bug fix for highlighting IPRED as a unknown variable.
;;    $ERROR IPRED=F
;;           Y    = IPRED+EPS(1)
;; 03-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Wed Nov  3 10:02:00 2010 (-0500) #1524 (Matthew L. Fidler)
;;    Made "FIRST "MAIN "LAST verbatim code highlight as "functions"
;; 03-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Wed Nov  3 09:58:40 2010 (-0500) #1522 (Matthew L. Fidler)
;;    Added more comment highlighting.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 09:32:09 2010 (-0500) #1516 (Matthew L. Fidler)
;;    Added highlighting of known variables 
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 09:21:27 2010 (-0500) #1511 (Matthew L. Fidler)
;;    Do not highlight known variables as undefined variables.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 21:26:20 2010 (-0500) #1506 (Matthew L. Fidler)
;;    Changed file coding system to us-ascii-unix.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 17:26:01 2010 (-0500) #1473 (Matthew L. Fidler)
;;    Verbatim code currently not color coded
;; 02-Nov-2010    Matthew L. Fidler
;;    Last-Updated: Tue Nov  2 16:02:07 2010 (-0500) #1460 (Matthew L. Fidler)
;;    Added a string font.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 14:50:38 2010 (-0500) #1440 (Matthew L. Fidler)
;;    Bug fix for RHS quantities. Same code as below.
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 14:04:17 2010 (-0500) #1422 (Matthew L. Fidler)
;;    Bug fix for identifying undefined variables
;;
;;    COM1=THETA(1);=Silly desc
;;    COM2=COM1+2;
;;
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 12:05:34 2010 (-0500) #1420 (Matthew L. Fidler)
;;    Font-lock option fix
;; 20-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 20 13:55:46 2010 (-0500) #1372 (Matthew L. Fidler)

;;    Added more checks for bad MUs.  Now also check for:
;;
;;      TVCL=THETA(1)
;;      CL=TVCL*DEXP(ETA(1))
;;      MU_5 = DLOG(TVCL)
;;
;; 14-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 14 13:45:31 2010 (-0500) #1351 (Matthew L. Fidler)
;;    Bug fix for undefined variables highlighting.
;; 14-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 14 11:26:17 2010 (-0500) #1340 (Matthew L. Fidler)
;;    Bug fix for unidentified records.
;; 13-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 13 08:30:46 2010 (-0500) #1095 (Matthew L. Fidler)

;;    Only do extended font-locking when outside of the $THETA $OMEGA and $SIGMA
;;    blocks.

;; 10-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 10 12:58:06 2010 (-0500) #1041 (Matthew L. Fidler)
;;    Made font-locking more customizeable.
;; 10-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 10 09:02:20 2010 (-0500) #1026 (Matthew L. Fidler)

;;    Bug fix for bad MUs that had MU_ underlined as a bad MU when there are no
;;    "bad mus"

;; 09-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Sep  9 16:42:01 2010 (-0500) #1012 (Matthew L. Fidler)
;;    Added highlighting of "Bad Mus" that is Mu_4+ETA(1) which should be
;;    MU_1+ETA(1)
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 10:31:57 2010 (-0500) #918 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 13-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Fri Aug 13 09:34:24 2010 (-0500) #768 (Matthew L. Fidler)
;;    Changed to only highlight in current Known records.
;; 13-Aug-2010    Matthew L. Fidler
;;    Last-Updated: Fri Aug 13 08:42:13 2010 (-0500) #755 (Matthew L. Fidler)
;;    Moved remaining font-lock variables from esn-mode.el
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
(eval-when-compile
  (require 'esn-vars)
  (require 'esn-reg))
(require 'esn-link)
(declare-function esn-get-inputs "esn-tables")
(declare-function esn-get-variable-names "esn-extended")
(declare-function esn-get-fixed-thetas "esn-properties")
(declare-function esn-rec "esn-properties")
(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-in-comment-p "esn-properties")
(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-space-input "esn-input")
(declare-function esn-narrow-to-current-problem "esn-narrow")
(declare-function esn-input-get-header "esn-input")


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra faces
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup esn-faces nil
  "EsN extra faces"
  :group 'esn-mode)

(defface esn-font-lock-constant-face-italic nil
  "Face for font lock italic functions"
  :group 'esn-faces)


(defface esn-font-lock-variable-name-face-italic nil
  "Face for font lock italic functions"
  :group 'esn-faces)

(defun esn-define-faces ()
  "Hook that defines the extra faces for EsN"
  (interactive)
  (copy-face 'font-lock-variable-name-face 'esn-font-lock-variable-name-face-italic)
  (set-face-attribute 'esn-font-lock-variable-name-face-italic nil :slant 'italic)
  (copy-face 'font-lock-constant-face 'esn-font-lock-constant-face-italic)
  (set-face-attribute 'esn-font-lock-constant-face-italic nil :slant 'italic))

(add-hook 'esn-mode-hook 'esn-define-faces)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock debug
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun esn-fld ()
  "* Debugs font-lock functions"
  (interactive)
  (let (
        (debug-on-error 't)
        (debug-on-quit 't))
    (mapc (lambda(x)
            (let ((tmp (format "%s" x)) fn)
              (when (string-match "(\\(esn[^ \n]*\\)" tmp)
                (setq tmp (match-string 1 tmp))
                (message "Looking at function %s" tmp)
                (save-excursion
                  (while (apply (intern tmp) (list (point-max)))
                    (message "Found match %s" (match-string 0)))))))
          font-lock-keywords
          )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual font-lock building function
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun esn-mode--fontlock (&optional lst)
  "Fontlock building"
  (interactive)
  (let (
        rlst
        )
    (setq rlst
          (list

           ;; Operators
           '(esn-symbols-non-words
             )
           ;; Symbols definition
           '(esn-symbols-superscripts
             )
           '(esn-symbols-subscripts
             )
           '(esn-symbols-subscripts-a_0
             )
           '(esn-symbols-words 
             )
           '(esn-symbols-make-partial
             )
           ;; Linking
           '(esn-make-link-data-include)
           '(esn-make-link-outfiles)
           '(esn-make-link-outfiles-2)
           '(esn-make-link-msfo)
           '(esn-make-link-msfo-2)
           
           '(esn-adaptive-font-lock-num-data
             (1 font-lock-comment-face)
             (2 font-lock-warning-face)
             (3 font-lock-comment-face)
             (4 font-lock-comment-face)
             (5 font-lock-comment-face))
           '(esn-adaptive-font-lock-num-vars
             (1 font-lock-comment-face)
             (2 font-lock-warning-face)
             (3 font-lock-comment-face)
             (4 font-lock-comment-face)
             (5 font-lock-comment-face))
           '("\\(;.*\\)"
            (1 font-lock-comment-face))
           '("^[ \t]*\\([Cc][ \t]+[^=\n].*\\)"
             (1 font-lock-comment-face))
           '("^[ \t]*\\([*!].*\\)"
             (1 font-lock-comment-face))
           '("^[ \t]*[\"]\\([*!Cc].*\\)"
             (1 font-lock-comment-face))
           '("^[ \t]*[\"][ \t]*\\([Ff][Ii][Rr][Ss][Tt]\\|[Mm][Aa][Ii][Nn]\\|[Ll][Aa][Ss][Tt]\\)[ \t]*$"
             (1 font-lock-function-name-face))
           '("'\\(?:.*?[\\\\]'\\)*[^']*\\('\\|\\'\\)"
             (0 font-lock-string-face))
           '("\\<\\(\\$PRO\\(?:B\\(?:L\\(?:EM?\\)?\\)?\\)?\\)[ \t]+\\([^\n;]*\\)"
             (1 font-lock-function-name-face)
             (2 font-lock-comment-face))
           ;; Record Highlighting
          '(esn-font-lock-records
            (1 font-lock-function-name-face))
          '(esn-font-lock-unknown-records
            (1 font-lock-warning-face))
          ;; Option=VALUE highlighting.  Putting before Options highlighting takes care of the case:
          ;; $EST METH=ZERO MAXEVAL=
          '(esn-font-lock-known-option-values
            (1 font-lock-constant-face))
          ;; Options Highlighting
          '(esn-font-lock-known-options
            (1 font-lock-builtin-face))
          ;; Input record highlighting
          '(esn-font-lock-input-data-items-over-4
            (1 font-lock-warning-face))
          '(esn-font-lock-over-data
            (0 font-lock-warning-face))
          '(esn-font-lock-input-data-items
            (0 font-lock-variable-name-face))
          ;; Abbreviated Records highlighted
          '(esn-font-lock-operators
            (1 font-lock-builtin-face))
          '(esn-font-lock-lhs
            (1 font-lock-variable-name-face)
            (2 font-lock-constant-face))
          '(esn-font-lock-rhs
            (1 font-lock-variable-name-face)
            (2 font-lock-constant-face))
          '(esn-font-lock-known-vars
            (0 font-lock-variable-name-face))
          '(esn-font-lock-lhs-warning
            (0 font-lock-warning-face))
          '(esn-font-lock-abbrev-forbidden
            (0 font-lock-warning-face))
          ;; Fixed theta highlighting
          '(esn-font-lock-fixed-theta)
	  ;; Highlighting fixed duration (in red)
	  '(esn-font-lock-fixed-duration)
          ;; Bad mu highlighting
          '(esn-font-lock-bad-mu
            (0 font-lock-warning-face))
          ;; Extended variable highlighting
	  '(esn-highlight-extended
	    (0 font-lock-variable-name-face))
          ;; Continue abbreviated highlighting
           '(esn-font-lock-abbrev-undefined
             (0 font-lock-warning-face))))
    (if (not lst)
        (progn
          (setq esn-font-lock-keywords rlst)
          (set (make-local-variable 'font-lock-defaults)
               '(esn-font-lock-keywords nil t))))
    (symbol-value 'rlst)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixed Duration (doesn't usually make sense)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom esn-highlight-fixed-durations t
  "Highlights fixed durations by making them red."
  :type 'boolean
  :group 'esn-face)

(defun esn-font-lock-fixed-duration (limit)
  "Font lock to highlight fixed durations"
  (interactive (list (point-max)))
  (condition-case error
      (if esn-highlight-fixed-durations
	  (let* ((reg (concat "^[ \t]*\\(\\(?:TV?\\)?D[0-9]+\\)[ \t]*=[ \t]*THETA("
			      (regexp-opt (mapcar (lambda(x) (number-to-string x))
                                                  (esn-get-fixed-thetas)) 't)
			      ")[ \t]*$"))
		 (ret (re-search-forward reg limit t)))
	    (when ret
	      (add-text-properties (match-beginning 1) (match-end 1)
				   '(face font-lock-warning-face)))
	    ret)
	nil)
    (error
     (message "[EsN] Error when running `esn-font-lock-fixed-duration': %s" (error-message-string error)))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fixed Theta
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom esn-highlight-fixed-thetas t
  "Highlights fixed thetas by italicising them."
  :type 'boolean
  :group 'esn-face)
(defun esn-font-lock-fixed-theta (limit)
  "Font lock to highlight fixed thetas.  Only find items where THETA is already highlighted."
  (interactive (list (point-max)))
  (condition-case error
      (if esn-highlight-fixed-thetas
	  (let* ((fixed (concat "\\<\\(THETA\\)(\\(" 
				(regexp-opt (mapcar (lambda(x) (number-to-string x))
						    (esn-get-fixed-thetas)) 't) "\\))"))
		 (ret (re-search-forward fixed limit t)))
	    (when ret
	      (when (and (match-string 1)
			 (match-string 2)
			 (eq (get-text-property (- (match-end 1) 1) 'face) 'font-lock-variable-name-face))
		(add-text-properties (match-beginning 1) (match-end 1)
				     '(face esn-font-lock-variable-name-face-italic))
		(add-text-properties (match-beginning 2) (match-end 2)
				     '(face esn-font-lock-constant-face-italic))))
	    ret)
	nil)
    (error
     (message "[EsN] Error in `esn-font-lock-fixed-theta': %s" (error-message-string error))
     nil)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Improper Mu-variable referencing
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-get-bad-mus ()
  "* Gets Bad Mu variables from the control stream"
  (if esn-font-lock-bad-mus-cached
      esn-font-lock-bad-mus-save
    (let (
          (abbrev (esn-rec esn-current-abbrev-records-regexp 't))
          (case-fold-search 't)
          tv mu
          (ret '()))
      (with-temp-buffer
        (insert abbrev)
        (goto-char (point-min))
        (while (re-search-forward ";.*" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (while (re-search-forward "[ \t(]MU_\\([0-9]+\\)[ \t]*[+][ \t]*ETA([ \t]*\\([0-9]+\\)[ \t]*)" nil t)
          (unless (string= (match-string 1) (match-string 2))
            (add-to-list 'ret (list (match-string 1) (match-string 2)))))
        (goto-char (point-min))
        (while (re-search-forward "[ \t(]ETA([ \t]*\\([0-9]+\\)[ \t]*)[ \t]*[+][ \t]*MU_\\([0-9]+\\)" nil t)
          (unless (string= (match-string 1) (match-string 2))
            (add-to-list 'ret (list (match-string 2) (match-string 1)))))
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*MU_\\([0-9]+\\)[ \t]*=[ \t]*.*?\\<TV\\([A-Za-z0-9_]+\\>\\)" nil t) 
          (setq mu (match-string 1))
          (setq tv (match-string 2))
          (save-excursion
            (goto-char (point-min))
            (when (re-search-forward (format "^[ \t]*%s[ \t]*=[ \t]*.*?\\<ETA(\\([0-9]+\\))" (regexp-quote tv)) nil t)
              (unless (string= mu (match-string 1))
                (add-to-list 'ret (list mu (match-string 1))))))))
      (setq esn-font-lock-bad-mus-cached 't)
      (setq esn-font-lock-bad-mus-save ret)
      (symbol-value 'ret))))
(defun esn-font-lock-bad-mu (limit)
  "* Highlights \"Bad\" Mus"
  (interactive (list (point-max)))
  (if (not esn-highlight-bad-mu) 
      nil
    (let (
          (lst (esn-font-lock-get-bad-mus)))
      (if lst
          (re-search-forward (concat "\\<MU_" (regexp-opt (mapcar (lambda(x) (nth 0 x)) lst) 't)) limit 't)
        nil
        ))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock strings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-strings (limit &optional use-quote)
  "* Font lock strings.  Note that strings are '' or \"\" and may be escaped with \\.  An initial string"
  (interactive (list (point-max)))
  (let (
        (found-string nil)
        (q (if use-quote "\"" "'"))
        ret
        )
    (if (not use-quote)
        (setq ret (re-search-forward (format "%s\\(?:.*?\\\\%s\\)*[^%s]%s" q q q q) limit t)))
    (symbol-value 'ret)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operator font-lock
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-operators (limit)
  "* Highlight operators"
  (interactive (list (point-max)))
  (if esn-highlight-operators
      (re-search-forward esn-font-lock-operators-reg  limit 't)
    nil
    ))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look for undefined variables for font-lock
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-abbrev-undefined (limit)
  "* Looks for variables undefined in Abbreviated blocks on RHS"
  (interactive (list (point-max)))
  (if esn-highlight-abbrev-undefined 
      (let (
            (debug-on-error 't)
            (case-fold-search 't)
            (abbrev-rec esn-current-abbrev-records-regexp)
            (word-reg esn-current-records-word-reg)
            (known-reg esn-current-nm-vars)
            (all-rec (eval-when-compile (esn-reg-records)))
            (rec (esn-get-current-record))
            (rhs esn-current-abbrev-rhs-norec)
            (inp (esn-get-inputs :return 'regexp))
            reg
            ret
            test
            looking
            md
            pt
            tmp0
            tmp
            )
        (save-excursion
          (save-match-data
            (setq looking (string-match abbrev-rec (concat "$" rec)))
            (setq reg (if looking word-reg abbrev-rec))
            (setq test (re-search-forward reg limit 't))
            (while test
              (setq pt (point))
              (setq md (match-data))
              (setq tmp0 (match-string 0))

              (if looking
                  (if (string-match all-rec tmp0)
                      (if (string-match abbrev-rec tmp0)
                          (progn
                            ;;Found Abbreviated Record, keep going.
                            (setq rec (esn-rec3 tmp0)))
                        ;; Found another record, switch to abbreviated record
                        (setq looking nil)
                        (setq reg abbrev-rec))
                    ;; Found a candidate
                    (cond
                     ( (looking-back "^[ \t]*\".*")
                       ;; Quoted variables.  Not an issue.
                       )
                     ( (save-match-data (looking-at "=[^=;\n]*\\(?:;.*\\)?$"))
                       ;; Left-handed not an issue
                       )
                     ( (save-match-data (looking-at ".*[^=/><;]=\\([^=]\\|$\\)"))
                       ;; Left-handed; Not an issue.
                       )
                     ( (looking-at "([ \t]*[0-9]+[ \t]*)")
                                        ;Skip A(x) type values
                       )
                     ( (string-match esn-font-lock-operators-reg tmp0)
                       ;; Not an undefined value.
                       )
                     ( (esn-in-comment-p)
                       ;; Comment, skip
                       )
                     ( (and (progn (setq tmp (assoc rec rhs))
                                   (symbol-value 'tmp))
                            (string-match (cadr tmp) tmp0))
                       ;; A Defined RHS quantity.
                       )
                     ( (and inp (string-match inp tmp0))
                       ;; Input Information, skip
                       )
                     ( (save-excursion
                         (let ( ret tmp)
                           (beginning-of-line)
                           (save-excursion
                             (when (re-search-backward (format "^[ \t]*%s[ \t]*=[^=;\n]*\\(?:;.*\\)?$" (regexp-quote tmp0)) nil 't)
                               (setq tmp (concat "$" (esn-get-current-rec)))
                               (when (string-match abbrev-rec tmp)
                                 (setq ret 't))))
                           (unless ret
                             ;; For cases such as
                             ;; $ERROR IPRED = F                           
                             ;;         Y    = IPRED * (1+EPS(1)) + EPS(2) 
                             (setq ret (re-search-backward (format "%s[ \t]*%s[ \t]*=[^=;\n]*\\(?:;.*\\)?$" abbrev-rec(regexp-quote tmp0)) nil 't)))
                           (symbol-value 'ret)))
                       ;; Already defined
                       )
                     ( (let ((ret (assoc (esn-get-current-rec) esn-current-nm-vars)))
                         (when ret
                           (setq ret (string-match (nth 1 ret) tmp0)))
                         (symbol-value 'ret)) ;; Known variable.
                       
                       )
                     ( 't
                       ;; Not defined
                       (setq ret test))))
                ;; Found Abbreviated record
                (setq looking 't)
                (setq reg word-reg)
                (setq rec (esn-rec3 tmp0)))
              ;; Next search
              (if ret
                  (setq test nil)
                (setq test (re-search-forward reg limit 't))))))
        (when ret
          (goto-char pt)
          (set-match-data md))
        (symbol-value 'ret))
    nil
    ))
(defun esn-font-lock-abbrev-forbidden (limit)
  "* Warning for forbidden variables"
  (interactive (list (point-max)))
  (if esn-highlight-abbrev-forbidden
      (esn-font-lock-lhs limit nil esn-current-abbrev-no nil nil 't)
    nil
    ))
(defun esn-font-lock-lhs-warning (limit)
  "* Warning for LHS that shouldn't be assigned a value"
  (interactive (list (point-max)))
  (if esn-highlight-lhs-warning
      (esn-font-lock-lhs limit ' nil esn-current-abbrev-rhs)
    nil
    ))
(defun esn-font-lock-rhs (limit)
  "* Font lock abbreviated code RHS"
  (interactive (list (point-max)))
  (if esn-highlight-rhs
      (esn-font-lock-lhs limit 't esn-current-abbrev-rhs)
    nil
    ))
(defun esn-font-lock-known-vars (limit)
  "* Font lock known common variables."
  (interactive (list (point-max)))
  (if esn-highlight-known-vars
      (esn-font-lock-lhs limit nil esn-current-nm-vars nil nil 't)
    nil
    ))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight known left-handed expressions in abbreviated code
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-lhs (limit &optional test-rhs lst highlight-alone highlight-both highlight-any exception-lst)
  "* Font lock abbreviated code LHS"
  (interactive (list (point-max)))
  (if (or test-rhs lst highlight-alone highlight-both highlight-any exception-lst
          (and (not test-rhs) (not lst) (not highlight-alone) (not highlight-both) (not highlight-any) (not exception-lst)
               esn-highlight-lhs
               ))
      (let (
            (case-fold-search 't)
            (all-rec (eval-when-compile (esn-reg-records)))
            (abbrev-rec esn-current-abbrev-records-regexp)
            (abbrev-lhs (or lst esn-current-abbrev-lhs))
            (rec (esn-get-current-record))
            (other-lst exception-lst)
            match-other
            rhs
            lhs
            reg
            test
            tmp
            looking
            pt
            md
            ret
            )
        (save-excursion
          (save-match-data
            (setq looking (assoc rec abbrev-lhs))
            (setq reg (if looking (cadr looking) abbrev-rec))
                                        ;        (message "Regular Expression 1:%s" reg)
            (setq test (re-search-forward reg limit 't))
            (while test
              (setq md (match-data))
              (setq pt (point))
              (setq tmp (match-string 0))
              (save-match-data
                (if looking
                    (if (string-match all-rec tmp)
                        (if (string-match abbrev-rec tmp)
                            (progn
                              ;; Found another abbreviated record
                              ;; Change regular expression
                              (setq rec (esn-rec3 (match-string 1 tmp)))
                              (setq reg (assoc rec abbrev-lhs))
                              (if  reg
                                  (setq reg (cadr reg))
                                ;; Can't find information about this abbreviated
                                ;; record, look for a new abbreviated record
                                (setq rec abbrev-rec)
                                (setq looking nil)))
                          ;; Found an non-abbreviated record
                          (setq reg abbrev-rec)
                          (setq looking nil))
                      ;; Found variable.  See what side of the equation its on
                      ;; Note logical operators of NONMEM 7 == /= >= <=
                      (setq lhs (or (looking-at ".*[^=/><;]=[^=]")
                                    (looking-at "[ \t]*=[^=;\n]*\\(?:;.*\\)?$")))
                      (setq rhs (save-excursion (re-search-backward "[^=/><;]=[^=].*?\\=" nil t)))
                      (when (and other-lst (assoc rec other-lst))
                        (setq match-other (string-match (cadr (assoc rec other-lst)) tmp)))
                      (cond
                       ( (looking-back "^[ \t]*\".*")
                         ;; Quoted variables.  Not an issue.
                         )
                       ( highlight-any
                         (when (or (not other-lst) (and other-lst (not match-other)))
                           (setq ret test)))
                       ( (and lhs (not rhs))
                         ;; LHS
                         (unless test-rhs
                           ;; Found Left-side expression, highlight
                           (when (or (not other-lst) (and other-lst (not match-other)))
                             (setq ret test))))
                       ( (and (not lhs) rhs)
                         ;; RHS
                         (when test-rhs
                           (when (or (not other-lst) (and other-lst (not match-other)))
                             (setq ret test))))
                       ( (and (not lhs) (not rhs))
                         ;; Expression on its own; Highlight both RHS and LHS expressions.
                         (when highlight-alone
                           (when (or (not other-lst) (and other-lst (not match-other)))
                             (setq ret test))))
                       ( (and rhs lhs)
                         ;; Invalid NONMEM line.
                         (when highlight-both
                           (when (or (not other-lst) (and other-lst (not match-other)))
                             (setq ret test))))))
                  ;; Found Abbrev Record
                  (setq rec (esn-rec3 tmp))
                  (setq reg (cadr (assoc rec abbrev-lhs)))
                  (setq looking 't)))
              ;; Now search for next item
              (if ret
                  (progn
                    (setq test nil)
                    ;; Save THETA(x) match data as two matches.
                    (if (not (string-match "([ \t]*[0-9]+[ \t]*)" tmp))
                        (progn
                          (backward-char (length tmp))
                          (looking-at (format "\\(.\\{%s\\}\\)\\(\\)" (length tmp)))
                          (setq md (match-data)))
                      (backward-char (length tmp))
                      (looking-at (format "\\(.\\{%s\\}\\)(\\([ \t]*[0-9]+[ \t]*\\))" (- (length tmp) (length (match-string 0)))))
                      (setq md (match-data))))
                (setq test (re-search-forward reg limit 't))))))
        (when ret
          (goto-char pt)
          (set-match-data md))
        (symbol-value 'ret))
    nil
    ))
(defun esn-font-lock-over-data (limit)
  "* Font lock over the amount of variables in the $DATA statement"
  (interactive (list (point-max)))
  (if esn-highlight-input-items-over-data
      (let (
            (case-fold-search 't)
            (inp-reg (eval-when-compile (esn-reg-record-exp "INP")))
            (all-rec (eval-when-compile (esn-reg-records)))
            (rec (esn-get-current-record))
            looking
            problem
            reg
            ret
            pt
            md
            tmp
            test
            )
        (save-excursion
          (save-match-data
            (setq looking (string= rec "INP"))
            (when looking
              (esn-space-input))
            (setq reg (if looking esn-number-input-warning-reg inp-reg))
            (unless reg
              (setq reg (eval-when-compile (esn-reg-record-exp "PRO")))
              (setq looking nil))
            (setq test (re-search-forward reg limit 't))
            (while test
              (setq pt (point))
              (setq md (match-data))
              (setq tmp (match-string 0)) 
              (if looking
                  (if (string-match all-rec tmp)
                      (if (string-match inp-reg tmp)
                          (progn
                            (esn-space-input)
                            (setq reg esn-number-input-warning-reg)
                            ;; Skip problem statement if nothing is "wrong"
                            (unless reg
                              (setq reg (eval-when-compile (esn-reg-record-exp "PRO")))
                              (setq looking nil))
                            ;; Don't change anything else.
                            )
                        ;; Found another record.
                        (setq looking nil)
                        (setq reg inp-reg))
                    ;; Found problem data item
                    (setq ret test))
                ;; Found either PROBLEM or INPUT record
                (if (string= "PRO" (esn-rec3 tmp))
                    (setq reg inp-reg)
                  (setq reg esn-number-input-warning-reg)
                  (setq looking 't)
                  ;; Skip problem statement if nothing is "wrong"
                  (unless reg
                    (setq reg (eval-when-compile (esn-reg-record-exp "PRO")))
                    (setq looking nil))))
              (if ret
                  (if (save-excursion (goto-char  pt) (esn-in-comment-p))
                      (progn
                        (setq ret nil)
                        (setq test (re-search-forward reg limit 't)))
                    (setq test nil))
                (setq test (re-search-forward reg limit 't))))))
        (when ret
          (goto-char pt)
          (set-match-data md))
        (symbol-value 'ret)))
  nil
  )


(defun esn-font-lock-get-data-cols ()
  "* This gets and assigns the number of data columns if inputting a CSV file."
  (save-restriction
    (save-excursion
      (let (
            (case-fold-search 't)
            (data "")
            (rcdir esn-completing-current-directory)
            (header "")
            (inhibit-read-only 't)
            (esw esn-skip-wrap)
            ret
            )
        (save-excursion
          (esn-narrow-to-current-problem)
          (goto-char (point-min))
          (if (not (re-search-forward "\\<\\$DATA? *\\([^ \n]*\\)" nil t)) nil
            (setq data (match-string 1))
            (if (string-match (format "^%s" (regexp-quote esn-completing-current-directory)) data)
                (setq data (replace-match "" nil nil data)))
            (unless (string= esn-font-lock-input-data data)
              (setq header (split-string (esn-input-get-header data) "[,; \t]"))
              (setq esn-font-lock-input-data data)
              (setq esn-font-lock-input-number (length header)))))
        (symbol-value 'esn-font-lock-input-number)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight $INPUT variables over 4 (NONMEM 6)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-input-data-items-over-4 (limit)
  "* Highlight $INPUT items over 4 for NONMEM 6."
  (interactive (list (point-max)))
  (if esn-highlight-input-items-over-4
      (let (
            (case-fold-search 't)
            (inp-rec (eval-when-compile (esn-reg-record-exp "INP")))
            (rec (esn-get-current-record))
            (reg-over (eval-when-compile (format "\\(\\<[A-Z][A-Z0-9_]\\{4,\\}\\|%s\\)" (esn-reg-records))))
            (all-rec (eval-when-compile (esn-reg-records)))
            md
            ret
            reg
            pt
            looking
            tmp
            (nm-ver (esn-update-get-version))
            test
            )
        (when (string= "-1" nm-ver)
          (setq nm-ver esn-assumed-version))
        (setq nm-ver (string-to-number nm-ver))
        (when (< nm-ver  7)
          (save-match-data
            (save-excursion
              (if (string= rec "INP")
                  (progn
                    (setq reg reg-over)
                    (setq looking 't))
                (setq reg inp-rec))
              (setq test (re-search-forward reg limit 't))
              (while test
                (setq pt (point))
                (setq tmp (match-string 1))
                (if looking
                    (if (string-match all-rec tmp)
                        (if (string-match inp-rec tmp)
                            (progn
                              ;; Input record, keep same.
                              )
                          ;; New record, search for $INPUT record.
                          (setq reg inp-rec)
                          (setq looking nil))
                      ;; Found
                      (backward-char (length tmp))
                      (setq ret (looking-at "[A-Z][A-Z0-9_]\\{3\\}\\([A-Z0-9_]+\\)"))
                      (setq md (match-data)))
                  ;; Found $INPUT record
                  (setq looking 't)
                  (setq reg reg-over))
                (if ret
                    (setq test nil)
                  (setq test (re-search-forward reg limit 't))))))
          (when ret
            (goto-char pt)
            (set-match-data md)))
        (symbol-value 'ret)))
  nil
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight reserved data items in NONMEM $INPUT record
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-input-data-items (limit)
  "* Highlight function for NONEMEM reserved data items in $INPUT record"
  (interactive (list (point-max)))
  (let* (
         (case-fold-search 't)
         (all-rec (eval-when-compile (esn-reg-records)))
         (dat-itm esn-current-input-data-items)
         (inp-rec (eval-when-compile (esn-reg-record-exp "INP")))
         (rec (esn-get-current-record))
         reg
         test
         tmp
         looking-for-opt
         pt
         md
         ret
         )
    (save-excursion
      (save-match-data
        (if (string= rec "INP")
            (progn
              (setq reg dat-itm)
              (setq looking-for-opt 't))
          (setq reg inp-rec))
        (setq test (re-search-forward reg limit 't))
        (while test
          (setq tmp (match-string 1))
          (setq pt (point))
          (setq md (match-data))
          (save-match-data
            (if looking-for-opt
                (if (string-match all-rec tmp)
                    (if (string-match (eval-when-compile
                                        (format "^%s$"
                                                (esn-reg-record-exp "INP"))) 
                                      tmp)
                        (progn
                          ;; Found $INPUT record; do nothing.
                          )
                      ;; Found another record; look for next input record.
                      (setq reg inp-rec)
                      (setq looking-for-opt nil))
                  ;; Found Data Item
                  (setq ret test))
              ;; Found Input record
              (setq looking-for-opt 't)
              (setq reg dat-itm)))
          (if ret
              (setq test nil)
            (setq test (re-search-forward reg limit 't))))))
    (when ret
      (goto-char pt)
      (set-match-data md))
    (symbol-value 'ret)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adaptive font Lock for Variables DATA x (=y)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-adaptive-font-lock-num-data (limit)
  "* Defines adaptive-font-lock to highlight automatically
generated counts of variables that are over the limit."
  (interactive (list (point-max)))
  (let* (
         (case-fold-search 't)
	(ret (re-search-forward
	      (format "\\(;%s.*?Data *\\)\\([0-9]+\\)\\([ \t]*(=\\)\\([0-9]+\\)\\() *%s;.*\\)"
		      esn-sub-begin
		      esn-sub-end)
	      limit
	      't))
	(n1 (if ret (string-to-number (match-string 2)) nil))
	(n2 (if ret (string-to-number (match-string 4)) nil)))
    (when n1
      (when (= n1 n2)
	(backward-char (length (match-string 0)))
	(setq ret (re-search-forward
		   (format "\\(;%s.*?Data *\\)\\(\\)\\([0-9]+\\)\\([ \t]*(=[0-9]+)\\)\\( *%s;.*\\)"
			   esn-sub-begin
			   esn-sub-end)
		   limit
		   't))))
    (symbol-value 'ret)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adaptive font lock for Variables X/Y.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-adaptive-font-lock-num-vars (limit)
  "* Defines adaptive-font-lock to highlight automatically
generated counts of variables that are over the limit."
  (interactive (list (point-max)))
  (let* (
         (case-fold-search 't)
	(ret (re-search-forward
	      (format "\\(;%s.*?Variables *\\)\\([0-9]+\\)\\(/\\)\\([0-9]+\\)\\( *%s;.*\\)"
		      esn-sub-begin
		      esn-sub-end)
	      limit
	      't))
	(n1 (if ret (string-to-number (match-string 2)) nil))
	(n2 (if ret (string-to-number (match-string 4)) nil)))
    (when n1
      (unless (> n1 n2)
	(backward-char (length (match-string 0)))
	(setq ret (re-search-forward
		   (format "\\(;%s.*?Variables *\\)\\(\\)\\([0-9]+/\\)\\([0-9]+\\)\\( *%s;.*\\)"
			   esn-sub-begin
			   esn-sub-end)
		   limit
		   't))))
    (symbol-value 'ret)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight Extended Variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-highlight-extended (limit &optional the ome sig)
  "Font Lock function to highlight extended variables."
  (interactive (list (point-max)))
  (if (not esn-wfn-color-extended)
      nil
    ;; Don't do highlighting unless $THETA, $OMEGA or $SIGMA is present.
    (if (not  (or (esn-rec "THE")
                  (esn-rec "OME")
                  (esn-rec "SIG")))
        nil
      ;; Also don't do any highlighting while in $THETA, $OMEGA or $SIGMA records
      (if (string-match "\\(THE\\|OME|\|SIG\\)" (esn-get-current-rec))
          nil
    (let (
	  (rexp (format "\\<\\(%s\\)\\>"
			(regexp-opt (append
				     (esn-get-variable-names "THE")
				     (esn-get-variable-names "OME")
				     (esn-get-variable-names "SIG")))))
	  (tmp "")
	  (pt (point))
	  (found nil)
	  (md nil)
	  (ret nil)
          (case-fold-search 't))
      (if  (string= rexp "\\<\\(\\)\\>")
          nil
        (save-excursion
	(setq ret (re-search-forward rexp limit t))
	(setq md (match-data))
	;; Not before an assignment operator =
	;; Only highlight if AFTER equals sign, or on a line by itself.
	(while (and ret (not found))
	  (while (and ret (save-excursion
			    (let (
				  (ret 't))
			      (if (re-search-backward "=.*\\=" nil t)
				  (setq ret nil)
				(when (re-search-forward "\\=[^=\n;]*\\(?:;.*\\)?$" nil t)
				  (setq ret nil)))
			      (symbol-value 'ret))))
	    (setq ret (re-search-forward rexp limit t))
	    (setq md (match-data))
	    (when (not ret)
	      (goto-char pt)))
	  (when ret
	    (set-match-data md)
	    (setq tmp (match-string 1))
	    (save-excursion
	      (beginning-of-line)
	      (setq found (not (re-search-backward (format "^\\(?:[ \t]*\\<\\$[A-Za-z][A-Za-z0-9]*\\)?[ \t]*%s[ \t]*=" (regexp-quote tmp)) nil t))))
	    (when (not found)
	      (setq ret (re-search-forward rexp limit t))
	      (setq md (match-data)))
	    (while (and ret (esn-in-comment-p))
	      (setq ret (re-search-forward rexp limit t))
	      (setq md (match-data))))
	  (when found
	    (setq found (not (looking-at "[^=;\n]*=")))
	    (when (not found)
	      ;; Fix double equals freeze.
	      (save-excursion
		(setq found (re-search-backward "=.*\\=" nil t)))))
	  (when (and  found)
	    (set-match-data md)
	    (setq pt (point)))))
      (when ret
	(goto-char pt))
      (symbol-value 'ret)))))))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font Lock Option Values
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-known-option-values (limit)
  "Defines adaptive font lock to only highlight option values that are suitable for the current record."
  (interactive (list (point-max)))
  (if esn-highlight-known-option-values
      (let (
            (case-fold-search 't)
            (rec-lst esn-current-records-which-options-have-val-reg)
            (val-lst esn-current-records-options-val-reg)
            (opt-rec-reg esn-current-records-which-have-val-reg)
            (all-rec (eval-when-compile (esn-reg-records)))
            looking-for-opt
            ret
            md
            pt
            tmp
            rec
            reg
            reg-val
            op
            test)
        (when esn-adaptive-font-lock
          (save-excursion
            (save-match-data
              (setq rec (esn-get-current-record))
              (setq looking-for-opt (assoc rec rec-lst))
              ;; Look for:

              ;; (1) Option that has values or next record, if current record has
              ;; options with values

              ;; (2) Next record with options that have values if current record
              ;; does not have options with values
              (setq reg (if looking-for-opt (cadr looking-for-opt) opt-rec-reg))
              (when looking-for-opt
                (setq looking-for-opt 't))
              (setq test (re-search-forward reg limit 't))
              (while test
                (setq pt (point))
                (setq md (match-data))
                (setq tmp (match-string 0))
                (if looking-for-opt
                    (if (string-match all-rec tmp)
                        (if (string-match opt-rec-reg tmp)
                            (progn
                              ;; Found another OPTION=VALUE record
                              ;; Change regular expression
                              (setq rec (esn-rec3 tmp))
                              (setq tmp reg)
                              (setq reg (cadr (assoc rec rec-lst)))
                              (unless reg
                                        ;                            (message "Record %s doesn't have options regular expression" rec)
                                (setq reg opt-rec-reg)
                                (setq looking-for-opt nil)))
                          ;; Found another record that does not have OPTION=VALUE
                          ;; type options
                          (setq rec (esn-rec3 tmp))
                          (setq looking-for-opt nil)
                          (setq reg opt-rec-reg))
                      ;; Found Option, Test to see if this option has a value that
                      ;; matches known values
                      (setq op (esn-rec3 tmp))
                      (setq reg-val (cadr (assoc op (cdr (assoc rec val-lst)))))
                      (when reg-val
                        (skip-chars-forward " \t=,")
                        (setq tmp (re-search-forward (format "\\=%s" reg-val) limit 't))
                        (when tmp
                          (setq pt (point))
                          (setq md (match-data))
                          (setq ret tmp))))
                  ;; Found options record; Change regular expression
                  (setq rec (esn-rec3 tmp))
                  (setq reg (cadr (assoc rec rec-lst)))
                  (setq looking-for-opt 't)
                  (unless reg
                                        ;                (message "Record %s doesn't have options regular expression" rec)
                    (setq reg opt-rec-reg)
                    (setq looking-for-opt nil)))
                ;; Catch some error that may have occured
                ;; Look for next possible candidate, or stop.
                (if ret
                    (setq test nil)
                  (setq test (re-search-forward reg limit 't)))))))
        (when ret
          ;; Forward to point, AND set match data of the test.
          (goto-char pt)
          (set-match-data md))
        ;; Otherwise retain position and match data and return nil.
        (symbol-value 'ret))
      nil
    ))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight Known Options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-known-options (limit)
  "Defines adaptive font lock to only highlight options that are
suitable for the current record."
  (interactive (list (point-max)))
  (let (
        (debug-on-error 't)
        (case-fold-search 't)
        (rec-lst esn-current-record-options-reg)
        (all-opts esn-current-all-options)
        (all-rec (eval-when-compile (esn-reg-records)))
        (opt-rec-reg esn-current-records-with-options)
        tmp
        pt
        ret
        test
        rec
        looking-for-opt
        md
        reg
        is-opt
        )
    (if (not esn-adaptive-font-lock)
	(progn
          ;; Look for ANY known option regardless of placement
	  (setq ret (re-search-forward all-opts limit 't))
	  (setq pt (point)))
      (save-excursion
        (save-match-data
          (setq rec (esn-get-current-record))
          (setq looking-for-opt (assoc rec rec-lst))
          ;; Look for:
          ;; (1) Option or next record, if current record has options
          ;; (2) Next record with options if current record does not have options.
          (setq reg (if looking-for-opt (cadr looking-for-opt) opt-rec-reg))
          (setq test (re-search-forward reg limit 't))
          (while test
            (setq md (match-data))
            (setq pt (point))
            (setq tmp (match-string 0))
            ;; Known option or something else?
            (when looking-for-opt
              (save-match-data
                (save-excursion
                  (goto-char (match-beginning 0)))))
            (save-match-data
              (if looking-for-opt
                  (if (string-match all-rec tmp)
                      (if (string-match opt-rec-reg tmp)
                          (progn
                            ;; Found another options Record
                            ;; Change regular expression
                            (setq rec (esn-rec3 (match-string 1 tmp)))
                            (setq reg (cadr (assoc rec rec-lst))))
                        ;; Found another record that does not have options
                        (setq looking-for-opt nil)
                        (setq reg opt-rec-reg))
                    ;; Found Option, return this option with match-data and point changed.
                    (setq ret test))
                ;; Found options record; Change regular expression.
                (setq rec (esn-rec3 (match-string 1)))
                (setq reg (cadr (assoc rec rec-lst)))
                (setq looking-for-opt 't)))
            ;; Look for next possible candidate, or stop.
            (if ret
                (setq test nil)
              (setq test (re-search-forward reg limit 't)))))))
    (when ret
      ;; Forward to point, AND set match data of the test.
      (goto-char pt)
      (set-match-data md))
    ;; Otherwise retain position and match data and return nil.
    (symbol-value 'ret)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock known records
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun esn-font-lock-records (limit)
  "* Font-Lock Records"
  (interactive (list (point-max)))
  (let ((case-fold-search 't))
    (re-search-forward esn-current-records-reg limit 't)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock unknown records
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-font-lock-unknown-records (limit)
  "* Find unknown records"
  (interactive (list (point-max)))
  (if esn-highlight-unknown-records
      (let (
            (case-fold-search 't)
            ret
            pt
            tmp
            found
            md
            )
        (save-excursion
          (save-match-data
            ;; Look for record
            (setq found (re-search-forward (eval-when-compile (esn-reg-records))  limit 't))
            (setq pt (point))
            (setq md (match-data))
            (while found
              (setq tmp (match-string 0))
              (if (string-match esn-current-records-reg tmp)
                  (progn
                    ;; Known record, skip.
                    (setq found (re-search-forward (eval-when-compile (esn-reg-records)) limit 't))
                    (setq pt (point))
                    (setq md (match-data)))
                ;; Unknown record, highlight.
                (setq ret found)
                (setq found nil)))))
        (when ret
          (set-match-data md)
          (goto-char pt))
        (symbol-value 'ret))
    nil))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add Symbols
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (compile load eval)
(defvar esn-symbol-words-regexp nil
  "* Words regular expression"
  )
(setq esn-symbol-words-regexp (eval-when-compile (format "\\<%s" (regexp-opt (mapcar (lambda(x) (nth 0 x)) esn-symbol-words-lst) 't))))
)
(defvar esn-symbol-non-words-regexp nil
  "* Non-words regular expression"
  )
(setq esn-symbol-non-words-regexp (eval-when-compile 
                                    (regexp-opt 
                                     (mapcar (lambda(x) (nth 0 x)) esn-symbol-non-words) 't)))
(defvar esn-symbol-partial 8706
  "* Partial Symbol"
  )
(defvar esn-symbol-superscripts 
  '(
    ("0" 8304)
    ("1" 185)
    ("2" 178)
    ("3" 179)
    ("4" 8308)
    ("5" 8309)
    ("6" 8310)
    ("7" 8311)
    ("8" 8312)
    ("9" 8313)
    ("+" 8314)
    ("-" 8315)
    ("=" 8316)
    ("(" 8317)
    (")" 8318)
    ("_" ?_))
  "* EsN superscripts"
  )
(defvar esn-symbol-subscripts
  '(
    ("0" 8320)
    ("1" 8321)
    ("2" 8322)
    ("3" 8323)
    ("4" 8324)
    ("5" 8235)
    ("6" 8326)
    ("7" 8327)
    ("8" 8328)
    ("9" 8329)
    ("+" 8330)
    ("-" 8331)
    ("=" 8332)
    ("(" 8333)
    (")" 8334)
    ("_" ?_))
  "* EsN subscripts"
  )
(defun esn-symbol-make-superscript (txt)
  "* Changes text 123 into superscript text"
  (esn-symbol-make-subscript txt esn-symbol-superscripts))
(defun esn-symbol-make-subscript (txt &optional symbol-list)
  "* Changes text 123 into subscript text (or superscript if symbol-list is different)"
  (let (
        (lst (or symbol-list esn-symbol-subscripts))
        (txt-lst)
        (txtn txt)
        (ret '()))
    (unless esn-symbols-expanded
      (while (string-match "[^0-9]+" txt)
        (setq txt (replace-match "" nil nil txt))))
    (setq txt-lst (split-string txt ""))
    (mapc
     (lambda(x)
       (when (assoc x lst)
         (push (nth 1 (assoc x lst)) ret)
         (push '(Br . Bl) ret)))
     txt-lst)
    (pop ret)
    (setq ret (reverse ret))
    (symbol-value 'ret)))
(defun esn-symbols-make-partial (limit)
  "* Makes parital expressions like dA3/dt"
  (interactive (list (point-max)))
  (if (not (and esn-symbols-dadt esn-use-symbols))
      nil
  (let (
        (ret (re-search-forward "\\<DADT\\(([0-9]+)\\)" limit 't))
        lst
        p1 p2
        (debug-on-error 't))
    (when ret
      (setq p1 (match-beginning 0))
      (setq p2 (match-end 0))
      (save-match-data
        (setq lst 
              (append (list esn-symbol-partial 
                            '(Br . Bl)
                            ?A '(Br . Bl))
                      (esn-symbol-make-subscript (match-string 1))
                      (list '(Br . Bl) ?\/ '(Br . Bl) esn-symbol-partial '(Br . Bl) ?t))))
      (compose-region p1 p2 lst 'decompose-region )))))
(defun esn-symbols-superscripts (limit)
  "* Make superscripts display nicely in text"
  (interactive (list (point-max)))
  (if (not (and esn-use-symbols esn-superscripts))
      nil
  (let (
        (ret (re-search-forward "[*][*]\\([0-9]+\\)\\>" limit 't)))
    (when ret
      (compose-region (match-beginning 0) (match-end 0)
                      (esn-symbol-make-superscript (match-string 0))
                      'decompose-region))
    (symbol-value 'ret))))

(defun esn-symbols-subscripts (limit)
  "* Compose subscripts"
  (interactive (list (point-max)))
  (if (not (and esn-use-symbols esn-subscripts))
           nil
  (let (
        (ret (re-search-forward "\\<\\([A-Za-z]+\\)+\\(_?\\)\\([0-9]+\\)\\>" limit 't)))
    (when ret
      (unless (or
               (and (string= (upcase (match-string 0)) "A_0") (save-match-data (looking-at "(")))
               (if esn-symbol-words
                   (save-match-data (string-match (eval-when-compile (format "%s$" esn-symbol-words-regexp)) (upcase (match-string 1))))
                 nil))
        (compose-region (match-beginning 2) (match-end 3)
                        (esn-symbol-make-subscript (concat (match-string 2) (match-string 3)))
                        'decompose-region))))))

(defun esn-symbols-non-words (limit)
  "* Composes non word-type symbols (like /= .NE.)"
  (interactive (list (point-max)))
  (if (not (and esn-use-symbols esn-operators))
      nil
    (let (
          (ret (re-search-forward esn-symbol-non-words-regexp limit 't))
          val
          u0
          pt
          )
      (when ret
        (setq u0 (upcase (match-string 0)))
        (cond 
         ( (string= "*" u0)
           (save-excursion
             (skip-chars-backward "*")
             (unless (save-match-data (looking-at "[*][*]+"))
               (setq val (assoc u0 esn-symbol-non-words))
               (when val
                 (compose-region (match-beginning 0) (match-end 0) 
                                 (nth 1 val) 
                                 'decompose-region
                                 )))))
         ( 't
           (setq val (assoc u0 esn-symbol-non-words))
           (when val
             (compose-region (match-beginning 0) (match-end 0) 
                             (nth 1 val) 
                             'decompose-region
                             )))))
      (symbol-value 'ret))))
(defun esn-symbols-words (limit)        
  "* Composes word-type symbols AND THETA(x) ETA(y), etc symbols"
  (interactive (list (point-max)))
  (if (not (and esn-use-symbols esn-symbol-words))
      nil
  (let (
        (ret (re-search-forward esn-symbol-words-regexp limit 't))
        val
        u0
        la
        p1
        cr
        )
    (when ret
      (setq u0 (upcase (match-string 0)))
      (setq val (assoc u0 esn-symbol-words-lst))
      (setq p1 (match-beginning 0))
      (save-match-data
        (setq la (looking-at "\\(([0-9]+)\\)"))
        (cond 
         ( (and la (or
                    (string= u0 "THETA") (string= u0 "ETA")
                    (string= u0 "ERR") (string= u0 "EPS")))
           (compose-region p1 (match-end 0)
                           (append (list (nth 1 val) '(Br . Bl))
                                   (esn-symbol-make-subscript (match-string 1)))
                           'decompose-region))
         ('t
          (setq la (looking-at "\\(_?[0-9]+\\)"))
          (cond
           ( la
             (compose-region p1 (match-end 0)
                             (append (list (nth 1 val) '(Br . Bl))
                                     (esn-symbol-make-subscript (match-string 1)))
                             'decompose-region))
           ( (looking-at "\\>")
             (setq cr 't))))))
      (when cr
        (compose-region (match-beginning 0) (match-end 0) 
                        (nth 1 val) 
                        'decompose-region
                        )))
    (symbol-value 'ret))))
(defun esn-symbols-subscripts-a_0 (limit)
  "* Compose A_0(n) and similar subscripts"
  (interactive (list (point-max)))
  (if (not (and esn-use-symbols esn-symbols-a_0))
      nil
  (let (
        (ret (re-search-forward 
              (if esn-symbol-words
                  (eval-when-compile 
                    (format "\\<%s\\((\\)\\([0-9]+\\)\\()\\)" 
                            (regexp-opt '(
                                          "A"
                                          "A_0"
                                          "CDEN_"
                                          "MTIME"
                                          "MPAST"
                                          "P"
                                          "THSIMP"
                                          "OMSIMP"
                                          ) 't)))
                (eval-when-compile 
                  (format "\\<%s\\((\\)\\([0-9]+\\)\\()\\)" 
                          (regexp-opt '(
                                        "THETA"
                                        "ETA"
                                        "EPS"
                                        "ERR"
                                        "A"
                                        "A_0"
                                        "CDEN_"
                                        "MTIME"
                                        "MPAST"
                                        "P"
                                        "THSIMP"
                                        "OMSIMP"
                                        ) 't)))) 
              limit 't))
        p1
        )
    (when ret
      (compose-region (match-beginning 2) (match-end 4)
                      (esn-symbol-make-subscript (concat 
                                                  (match-string 2)
                                                  (match-string 3)
                                                  (match-string 4)))
                      'decompose-region))
    (symbol-value 'ret))))

(defun esn-compose-region (p1 p2)
  "* Composes region from point p1 to point p2"
  (interactive (list (point-min) (point-max)))
  (let ((debug-on-error 't))
    (save-excursion
      (goto-char p1)
      (esn-symbols-superscripts p2)
      (goto-char p1)
      (esn-symbols-subscripts p2)
      (goto-char p1)
      (esn-symbols-subscripts-a_0 p2)
      (goto-char p1)
      (esn-symbols-words p2)
      (goto-char p1)
      (esn-symbols-non-words p2)
      (goto-char p1)
      (esn-symbols-make-partial p2))))
(provide 'esn-fontlock)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-fontlock.el ends here
