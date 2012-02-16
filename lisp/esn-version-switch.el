;;; esn-version-switch.el ---  Switch variables based on NONMEM version
;; 
;; Filename: esn-version-switch.el
;; Description: Switches color coding, help, etc based on the version of nonmem
;;              selected
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Fri Aug 20 09:29:13 2010 (-0500)
;; Version: 0.1
;; Last-Updated: Thu Feb 16 10:37:53 2012 (-0600)
;;           By: Matthew L. Fidler
;;     Update #: 124
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Resist the urge to make these variables buffer local.  It BREAKS
;; things currently.  As a TODO, if anyone wants to write a function
;; that makes the variables local and assigns them outside of a
;; temporary buffer that would be great.  Currently a known issue is
;; by changing the version on NONMEM in one buffer, you change it in
;; every buffer.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 11-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Dec 11 19:16:35 2010 (-0600) #109 (Matthew L. Fidler)
;;    Changed all the buffer-local variables to be global.  Should investigate if I could change them to just local variables with everything being fine.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 14:23:09 2010 (-0500) #93 (Matthew L. Fidler)
;;    On version switch, reset cache variable `esn-update-get-version-cache'
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 09:11:13 2010 (-0500) #91 (Matthew L. Fidler)
;;
;;    Added nm-vars.  For nm-vars-5 assume its the same as nm-vars-6.  NONMEM 5
;;    doesn't have variables.for in the help.
;;
;; 25-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 25 08:08:12 2010 (-0500) #80 (Matthew L. Fidler)
;;    Made the version switch turn off cached values
;; 24-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 24 11:55:13 2010 (-0500) #57 (Matthew L. Fidler)
;;    Added variable switch function
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
(defvar esn-current-nm-vars nil )
(defvar esn-current-fallback-help-version "7")
(defvar esn-current-version-selected "")

(defvar esn-current-nm-help-records nil)
(defvar esn-current-nm-help-records nil)
(defvar esn-current-nm-help-records-discussion nil)


(defvar esn-current-records-complete nil)
(defvar esn-current-record-complete-options nil)
(defvar esn-current-record-complete-option-values nil)

(defvar esn-current-records-with-options nil
  "* What is the current regular expression for records with options")

(defvar esn-current-record-options-reg nil
  "* List of regular expressions of options/records by record"
  )
(defvar esn-exact-current-records-reg nil)
(defvar esn-current-records-reg nil
  "* What is the current regular expression for records"
  )

(defvar esn-current-all-options nil
  "* What is the current regular expression for all known options (regardless of location)"
  )

(defvar esn-current-records-which-options-have-val-reg nil
  "* Regular expressions for which Options have Values")

(defvar esn-current-records-which-have-val-reg nil
  "* Regular expression for records with OPTION=VALUE"
  )
(defvar esn-current-records-options-val-reg nil
  "* Value list/regular expression")
(defvar esn-current-records-reg nil
  "* Regular Expression for records"
  )
(defvar esn-current-input-data-items  nil
  "* Reserved NONMEM data items"
  )
(defvar esn-current-abbrev-records-regexp nil
  "* Abbreviated  Records"
  )
(defvar esn-current-abbrev-lhs nil
  "* LHS variables"
  )

(defvar esn-current-abbrev-rhs nil
  "* RHS variables"
  )
(defvar esn-current-abbrev-no nil
  "* Forbidden variables")

(defvar esn-current-records-word-reg nil
  "* Expression for WORD or record"
  )
(defvar esn-current-abbrev-rhs-norec nil
  "* Expression for rhs alone")



(defvar esn-current-abbrev-records nil
  "* Abbreviated Records Codes")

;;;###autoload
(defun esn-current-record-options-help (&optional record version)
  "* Gets the current record options help vector"
  (let (
        (rec (or record (upcase (esn-get-current-rec))))
        (nm-ver (or version (esn-update-get-version)))
        (ret '())
        )
    (when (string= "-1" nm-ver)
      (setq nm-ver esn-assumed-version))
    (cond 
     ( (string= rec "")
       (setq nm-ver nil) 
       )
     ( (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  nm-ver ".el"))
       )
     ( (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  nm-ver ".el.gz"))
       )
     ( (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  nm-ver ".elc"))
       )
     ( (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  nm-ver ".elc.gz"))
       )
     ( (or (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  (number-to-string (floor (string-to-number nm-ver))) ".el"))
           (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  (number-to-string (floor (string-to-number nm-ver))) ".elc"))
           (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  (number-to-string (floor (string-to-number nm-ver))) ".elc.gz"))
           (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  (number-to-string (floor (string-to-number nm-ver))) ".el.gz"))
           )
       (setq nm-ver (number-to-string (floor (string-to-number nm-ver)))))
     ( (or 
        (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  esn-current-fallback-help-version ".el"))
        (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  esn-current-fallback-help-version ".elc"))
        (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  esn-current-fallback-help-version ".el.gz"))
        (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-help-" rec "-"  esn-current-fallback-help-version ".elc.gz"))
        )
       (setq nm-ver esn-current-fallback-help-version)
       )
     ('t
      (setq nm-ver nil)))
    (when nm-ver
      (with-temp-buffer
        (insert (concat "(require 'esn-nm-cookies-help-" rec "-" nm-ver ")\n"))
        (insert (concat "(setq ret esn-records-help-" rec "-" nm-ver ")\n"))
        (eval-buffer)))
    (symbol-value 'ret)))

;;;###autoload
()
(defun esn-switch-variables ()
  "* Switches the variables for a different version of NONMEM"
  (interactive)
  (setq esn-update-get-version-cache nil)
  (let (
        (nm-ver (esn-update-get-version))
        (ret '())
        (debug-on-error 't)
        (nm7code "\n
 (require 'esn-nm-cookies-7)
 (require 'esn-nm-cookies-complete-7)
 (require 'esn-nm-cookies-help-records-7)
 (require 'esn-nm-vars-7)
 (setq esn-current-nm-vars esn-nm-vars-7)
 (setq esn-current-nm-help-records esn-nm-help-records-7)
 (setq esn-current-nm-help-records esn-nm-help-records-7)
 (setq esn-current-nm-help-records-discussion esn-nm-help-records-discussion-7)


 (setq esn-current-records-complete esn-records-complete-7)
 (setq esn-current-record-complete-options esn-record-complete-options-7)
 (setq esn-current-record-complete-option-values esn-record-complete-option-values-7)

 (setq esn-current-records-with-options esn-records-with-options-reg-7)

 (setq esn-current-record-options-reg esn-record-options-reg-7)

 (setq esn-current-records-reg esn-records-reg-7)
 (setq esn-exact-current-records-reg esn-exact-records-reg-7)


 (setq esn-current-all-options esn-records-all-options-7)

 (setq esn-current-records-which-options-have-val-reg esn-records-which-options-have-val-reg-7)

 (setq esn-current-records-which-have-val-reg esn-records-which-have-val-reg-7)
 (setq esn-current-records-options-val-reg esn-records-options-val-reg-7)
 (setq esn-current-records-reg esn-records-reg-7)
 (setq esn-current-input-data-items  esn-input-data-items-7)
 (setq esn-current-abbrev-records-regexp esn-abbrev-records-regexp-7)
 (setq esn-current-abbrev-lhs esn-abbrev-lhs-7)

 (setq esn-current-abbrev-rhs esn-abbrev-rhs-7)
 (setq esn-current-abbrev-no esn-abbrev-no-7)

 (setq esn-current-records-word-reg esn-records-word-reg-7)
 (setq esn-current-abbrev-rhs-norec esn-abbrev-rhs-norec-7)
 (setq esn-current-abbrev-records esn-abbrev-records-7)
")
        )
    (when (string= "-1" nm-ver)
      (setq nm-ver esn-assumed-version))
    (message "Nm Version: %s" nm-ver)
    (cond 
     ( (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  nm-ver ".el")))
     ( (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  nm-ver ".el.gz")))
     ( (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  nm-ver ".elc")))
     ( (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  nm-ver ".elc.gz")))
     ( (or (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  (number-to-string (floor (string-to-number nm-ver))) ".el"))
           (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  (number-to-string (floor (string-to-number nm-ver))) ".elc"))
           (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  (number-to-string (floor (string-to-number nm-ver))) ".elc.gz"))
           (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  (number-to-string (floor (string-to-number nm-ver))) ".el.gz")))
       (setq nm-ver (number-to-string (floor (string-to-number nm-ver)))))
     ( (or 
        (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  esn-current-fallback-help-version ".el"))
        (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  esn-current-fallback-help-version ".elc"))
        (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  esn-current-fallback-help-version ".el.gz"))
        (file-exists-p (concat esn-path "etc/cookies/esn-nm-cookies-"  esn-current-fallback-help-version ".elc.gz")))
       (setq nm-ver esn-current-fallback-help-version))
     ('t
      (setq nm-ver nil)))
    (with-temp-buffer
      (unless (string= "7" nm-ver)
        (while (string-match "7" nm7code)
          (setq nm7code (replace-match nm-ver nil nil nm7code))))
      (when (string= "5" nm-ver)
        (while (string-match "vars-5" nm7code)
          (setq nm7code (replace-match "vars-6" nil nil nm7code))))
      (insert nm7code)
      (eval-buffer))
    (setq ac-source-esn-options-cached nil)
    (setq ac-source-esn-option-value-cached nil)
    (setq esn-current-completions nil)))


(esn-switch-variables)
(provide 'esn-version-switch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-version-switch.el ends here
