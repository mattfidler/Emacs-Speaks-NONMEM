;;; esn-cwres.el --- CWRES handling 
;; 
;; Filename: esn-cwres.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Aug 19 10:07:56 2010 (-0500)
;; Version: 
;; Last-Updated: Thu Apr 28 06:58:24 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 17
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
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 10:08:44 2010 (-0500) #1 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 19-Aug-2010    Matthew L. Fidler  
;;
;;    Added header and changed records to use standardized regular expressions.
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

(declare-function esn-xpose-run-number "esn-xpose")
(declare-function esn-get-extension "esn-xpose")
(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-is-foce-p "esn-properties")
(declare-function esn-is-focei-p "esn-properties")
(declare-function esn-is-hybrid-p "esn-properties")
(declare-function esn-is-fo-posthoc-p "esn-properties")
(declare-function esn-is-foi-posthoc-p "esn-properties")
(declare-function esn-is-lap-p "esn-properties")
(declare-function esn-rec "esn-properties")
(declare-function esn-max-eta "esn-properties")
(declare-function esn-max-eps "esn-properties")
(declare-function esn-is-pred-p "esn-properties")
(declare-function esn-max-theta "esn-properties")

(eval-when-compile
  (require 'esn-start))

;;;###autoload
(defun esn-quote-infn (arg)
  "* Quote INFN template to a regular expression."
  (let (
	(infn arg)
	(rep "\\\\([0-9]+\\\\)")
	(case-fold-search 't))
    (while (string-match "\\$INFN?[ \n\t]+" infn)
      (setq infn (replace-match "" nil nil infn)))
    (setq infn (regexp-quote infn))
    (while (string-match "\\([^/\\[]\\)\\(\\\\\\\\\\|/\\)" infn)
      (setq infn (replace-match "\\1[/\\\\\\\\]" nil nil  infn)))
    (while (string-match "(\\([^ ][^*]\\)" infn)
      (setq infn (replace-match "( *\\1" nil nil infn)))
    (while (string-match "\\\\\\*)" infn)
      (setq infn (replace-match "\\\\* *)" nil nil infn)))
    (while (string-match "\\([^*]\\))" infn)
      (setq infn (replace-match "\\1 *)" nil nil infn)))
    (while (string-match ") \\([^+]\\)" infn)
      (setq infn (replace-match ") +\\1" nil nil infn)))
    (while (string-match "\\(OPEN\\|WRITE\\)( \\*[0-9]+" infn)
      (setq infn (replace-match (format "\\1( *%s" rep)  nil nil infn))
      (setq rep "\\\\1"))
    (while (string-match "^  +" infn)
      (setq infn (replace-match " *" nil nil infn)))
    (while (string-match "\\([^*]\\)," infn)
      (setq infn (replace-match "\\1 *, *" nil nil infn)))
    (while (string-match "\n+\\([^+]\\)" infn)
      (setq infn (replace-match "\n+\\1" nil nil infn)))
    (while (string-match "IF *(" infn)
      (setq infn (replace-match "IF *(" nil nil infn)))
    (while (string-match ") *THEN" infn)
      (setq infn (replace-match ") *THEN" nil nil infn)))
    (while (string-match "\n\\+ \\*" infn)
      (setq infn (replace-match "[ \n]*\n *" nil nil infn)))
    (while (string-match "\n\\+$" infn)
      (setq infn (replace-match "" nil nil infn)))
    (while (string-match "\n\\+$" infn)
      (setq infn (replace-match "" nil nil infn)))
    (while (string-match "\n\\+" infn)
      (setq infn (replace-match "[ \n]*\n[ \n]*" nil nil infn)))
    (while (string-match "FILE *= *[^.].*?)" infn)
      (setq infn (replace-match "FILE *= *.*?)" nil nil infn)))
    (while (string-match "  +" infn)
      (setq infn (replace-match " +" nil nil infn)))
    (when (string-match "\\(DATA.*?NTH.*?NETA.*?NEPS.*?\\)\\[.*\\]" infn)
      (setq infn (replace-match "\\1 *[/\\\\\\\\] *[0-9]+ *, *[0-9+] *, *[0-9]+ *[/\\\\\\\\]"
				nil nil infn)))
    (while (string-match "[\n \t]" (substring infn -1))
      (setq infn (substring infn 0 -1)))
    (setq infn (concat "[\n \t]*" infn))
    (symbol-value 'infn)))
(defun esn-rep-infn (arg &optional ignoreFileHandle)
  "* Fills in the INFN template."
  (let (
	(infn arg)
	(sn (if (esn-use-xpose-p)
		(concat (if esn-xpose-small-table-file-names
			    "cw"
			  "cwtab") (esn-xpose-run-number) (esn-get-extension ".est"))
              (if (esn-use-plt-p)
                  "cwtab.est"
                (concat (progn
                          (let (
                                (fn (buffer-file-name)))
                            (when (and fn (string-match "\\.[^.]*$" fn))
                              (setq fn (replace-match "" nil nil fn)))
                            (when (and fn (string-match "[/\\]\\([^/\\]*\\)$" fn))
                              (setq fn (match-string 1 fn)))
                            (symbol-value 'fn))) esn-cwres-estimate-extension))))
            (num 49))
    ;; Get the next file handle.
    (unless ignoreFileHandle
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "OPEN *( *\\([0-9]+\\) *," nil t)
	  (setq num (max num (string-to-number (match-string 1))))))
      (setq num (+ 1 num))
      (while (string-match "\\(OPEN\\|WRITE\\) *( *[0-9]+ *," infn)
	(setq infn (replace-match "\\1 ( num ," nil nil infn)))
      (while (string-match "\\(OPEN\\|WRITE\\) ( num ," infn)
	(setq infn (replace-match (format "\\1(%s," num) nil nil infn))))
    ;; Add after PK block.
    (while (string-match "FILE *[= \t] *[^)].*?)" infn)
      (setq infn (replace-match "FILE=)" nil nil infn)))
    (while (string-match "FILE=)" infn)
      (setq infn (replace-match (format "FILE='%s%s%s')"
					esn-completing-current-directory
					(if esn-cwres-psn
					    "../../"
					  "")
					sn) 't 't infn)))
    (while (string-match "\\$INFN?\n*" infn)
      (setq infn (replace-match "" nil nil infn)))
    (while (string-match "^\n+" infn)
      (setq infn (replace-match "" nil nil infn)))
    (while (string-match "\n+$" infn)
      (setq infn (replace-match "" nil nil infn)))
    (setq infn (concat infn "\n"))
    (symbol-value 'infn)))
(defun esn-comres-max-com ()
  "* Returns the maximum number of COMs in the control stream."
  (let (
	(com 0)
	(curcom 0)
	(case-fold-search 't))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\<COM(\\([0-9]+\\))" nil t)
	(setq curcom (string-to-number (match-string 1)))
	(save-excursion
	  (if (re-search-backward ";.*\\=" nil t) nil
	    (if (> curcom com)
		(setq com curcom))))))
    (symbol-value 'com)))
(defun esn-comres-base-number ()
  "* This returns the base number defining Conditional Weighted Residuals. It also appropriately renumbers the associated COMs, and remove lines for \"LAST"
  (let (
	(maxcom (esn-comres-max-com))
	(curcom 1)
	(i 0)
	(comcwres nil)
	(case-fold-search 't))
    (while (< i maxcom)
      (setq i (+ i 1))
      (setq comcwres nil)
      (save-excursion
	(goto-char (point-min))
	(if (re-search-forward (format "^\" *COM(%s) *= *\\(G\\|H\\)H?([0-9]+\\,1).*\n*" i) nil t)
	    (progn
	      (setq comcwres 't)
	      (replace-match ""))))
      (if comcwres nil
	(if (not (= curcom i))
	    (save-excursion
	      (goto-char (point-min))
	      (while (re-search-forward (format "\\<COM(%s)" i) nil t)
		(replace-match (format "COM(%s)" curcom)))))
	(setq curcom (+ curcom 1))))
    (symbol-value 'curcom)))
(defun esn-rem-cwres ()
  "Remove applicable CWRES code."
  (interactive)
  (save-restriction
  (save-excursion
    (let (
	  (case-fold-search 't)
	  (inhibit-read-only 't)
	  (inhibit-point-motion-hooks 't))
      (goto-char (point-min))
      (when (or
	     (re-search-forward "COM([0-9]) *= *[GH][0-9]+1" nil t)
	     (re-search-forward "\\<[GH][0-9]+1" nil t))
	
	;; CWRES code is present.
	(goto-char (point-min))
	(while (re-search-forward "COM([0-9]) *= *[GH][0-9]+1" nil t)
	  (replace-match ""))
	(let (
	      (infn-1 esn-cwres-infn5-pred-template-1)
	      (infn-2 (esn-rep-infn esn-cwres-infn5-pred-template-2 't))
	      (infn-re (esn-quote-infn esn-cwres-infn-template))
	      (rep-infn nil))
	  (when (string-match "^\" *FIRST *\n?" infn-1)
	    (setq infn-1 (replace-match "" nil nil infn-1)))
	  ;; Replace INFN blocks.
	  (mapc (lambda(x)
		  (goto-char (point-min))
		  (while (re-search-forward x nil t)
		    (setq rep-infn 't)
		    (replace-match "")))
		(list infn-1 infn-2 infn-re))
	  ;; Renumber COMs.
	  (esn-comres-base-number)
	  (unless rep-infn
	    ;; Remove INFN in subroutines block.
	    (when (re-search-forward (eval-when-compile 
                                       (esn-reg-record-exp "SUB"))
                                     nil t)
	      (esn-narrow-rec)
	      (goto-char (point-min))
	      (if (re-search-forward "\\<INFN? *= *[^ ]*" nil t)
		  (replace-match ""))
	      (widen)))
	  ;; Now remove Empty $INFN
	  (goto-char (point-min))
	  (while (re-search-forward (eval-when-compile
                                      (esn-reg-record-exp "INF")) nil t)
	    (esn-narrow-rec)
	    (goto-char (point-min))
	    (re-search-forward (eval-when-compile
                                      (esn-reg-record-exp "INF")) nil t)
	    (skip-chars-forward " \t\n")
	    (when (eobp)
	      (delete-region (point-min) (point-max)))
	    (widen))
	  ;; Now remove Empty "LAST and "FIRST
	  (mapc (lambda(x)
		  (goto-char (point-min))
		  (while (re-search-forward
			 (format "\\(\" *%s *\n?\\)\\(\\(?:.\\|\n\\)*\\)" x) nil t)
		    (let (
			  (p1 (- (point) (length (match-string 0))))
			  (p2 (- (point) (length (match-string 2))))
			  (tmp (match-string 2))
			  (del nil))
		      (while (string-match ";.*?" tmp)
			(setq tmp (replace-match "" nil nil tmp)))
		      (with-temp-buffer
			(insert tmp)
			(goto-char (point-min))
			(skip-chars-forward " \t\n")
			(when (or (eobp) (not (looking-at "\"")))
			  ;; Delete "LAST or "FIRST,
			  (setq del 't)))
		      (when del
			(delete-region p1 p2)))))
		(list "LAST" "FIRST")))
	(esn-comres-base-number))))))
(defun esn-add-cwres ()
  "Add CWRES code."
  (let (
	(case-fold-search 't)
	(inhibit-read-only 't)
	(inhibit-point-motion-hooks 't))
    (save-excursion
      (esn-add-infn)
      (esn-add-abbrev))))

(defun esn-cwres ()
  "Conditionally Adds/Deletes Conditional Weighted Residual Code."
  (interactive)
  (let (
	(inhibit-read-only 't)
	(inhibit-point-motion-hooks 't))
  (save-excursion
    (if (or
	 (and esn-cwres-foce (esn-is-foce-p))
	 (and esn-cwres-focei (esn-is-focei-p))
	 (and esn-cwres-hybrid (esn-is-hybrid-p))
	 (and esn-cwres-fo-posthoc (esn-is-fo-posthoc-p))
	 (and esn-cwres-foi-posthoc (esn-is-foi-posthoc-p))
	 (and esn-cwres-lap (esn-is-lap-p)))
	(esn-add-cwres)
      (when esn-cwres-remove
	(esn-rem-cwres))))))
(defun esn-add-abbrev ()
  "* Adds the appropriate $ABBREV code if needed, and replace if not needed."
  (let (
	(nm-ver (esn-update-get-version)))
    (when (string= "-1" nm-ver)
      (setq nm-ver esn-assumed-version))
    (unless (>= (string-to-number nm-ver) 6.2)
      (if (esn-rec "DAT")
	  (let (
		(com (esn-generate-last)))
	    (if (not (esn-replace-in-record "\\<COMRES *= *[0-9]+" (format "COMRES=%s" com) "ABB"))
		(esn-insert-after (format "$ABBREVIATED COMRES=%s\n" com) (list "DAT"))))))))
(defun esn-generate-last ()
  "* Generates and adds the \"LAST code block for CWRES. Returns the number of COMS needed."
  (let (
	(last "\n\"LAST\n")
	(curcom (esn-comres-base-number))
	(maxeta (esn-max-eta))
	(maxeps (esn-max-eps))
	(pred (esn-is-pred-p))
	(nm-ver (esn-update-get-version))
	(h (if (esn-is-pred-p) "H" "HH"))
	(i 0))
    (when (string= "-1" nm-ver)
      (setq nm-ver esn-assumed-version))
    (when (or pred (esn-rec "ERR"))
      (while (< i maxeta)
	(setq i (+ i 1))
	(setq last (format "%s\" COM(%s)=G(%s,1)\n" last curcom i))
	(setq curcom (+ curcom 1)))
      (setq i 0)
      (while (< i  maxeps)
	(setq i (+ 1 i))
	(setq last (format "%s\" COM(%s)=%s(%s,1)\n" last curcom h i))
	(setq curcom (+ curcom 1)))
      (setq last (substring last 0 (- (length last) 1)))
      (setq last (concat last "\n"))
      (unless (>= (string-to-number nm-ver) 6.2)
	(if pred
	    (if (not (esn-replace-in-record "\n+\"LAST" last "PRE"))
		(esn-insert-after last (list "PRE")))
	  (if (not (esn-replace-in-record "\n+\"LAST" last "ERR"))
	      (esn-insert-after last (list "ERR")))))
      (setq curcom (- curcom 1))
      (symbol-value 'curcom))))
(defun esn-add-infn ()
  "* Adds the INFN block in the appropriate place if it isn't present."
  (interactive)
  (save-restriction
  (let* (
	 (infn esn-cwres-infn-template)
	 (infn-re (esn-quote-infn infn))
	 (nm-ver (esn-update-get-version))
	 (infn-rep ""))
    (when (string= "-1" nm-ver)
      (setq nm-ver esn-assumed-version))
    (when (string-match "^5\\(\\.\\|$\\)" nm-ver)
      (if (esn-is-pred-p)
	  (let (
		(infn-1 esn-cwres-infn5-pred-template-1)
		(infn-1-nofirst nil)
		(infn-2 (esn-rep-infn esn-cwres-infn5-pred-template-2 't))
		(theta.n (esn-max-theta))
		(eta.n (esn-max-eta))
		(eps.n (esn-max-eps)))
	    
	    (while (string-match "[ \n\t]" (substring infn-1 -1))
	      (setq infn-1 (substring infn-1 0 -1)))
	    (while (string-match "[ \n\t]" (substring infn-1 0 1))
	      (setq infn-1 (substring infn-1 1)))
	    
	    (while (string-match "[ \n\t]" (substring infn-2 -1))
	      (setq infn-2 (substring infn-2 0 -1)))
	    (while (string-match "[ \n\t]" (substring infn-2 0 1))
	      (setq infn-2 (substring infn-2 1)))
	    
	    (when (string-match "\\(DATA +NTH *, *NETA *, *NEPS *\\)/.*?/" infn-1)
	      (setq infn-1
		    (replace-match
		     (format "\\1/%s,%s,%s/" theta.n eta.n eps.n)
		     nil nil infn-1
		     )))
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward (eval-when-compile
                                         (esn-reg-record-exp "PRE")) nil t)
		(esn-narrow-rec)
		(if (string-match "^\" *FIRST *\n?" infn-1)
		    (setq infn-1-nofirst (replace-match "" nil nil infn-1))
		  (setq infn-1-nofirst infn-1))
		(goto-char (point-min))
		(if (re-search-forward
		     (concat "\n[ \t\n]*?"
			     (esn-quote-infn infn-1-nofirst)
			     "[ \t\n]*?\n") nil t)
		    (replace-match (concat "\n" infn-1-nofirst "\n"))
		  (when (re-search-forward (eval-when-compile
                                             (esn-reg-record-exp "PRE")) nil t)
		    (insert "\n")
		    (insert infn-1)))
		(goto-char (point-min))
		(if (re-search-forward
		     (concat "[ \t\n]*?"
			     (esn-quote-infn infn-2)
			     "[ \t\n]*?") nil t)
		    (replace-match (concat "\n" infn-2))
		  (goto-char (point-max))
		  (re-search-backward "\" *LAST *$" nil t)
		  (insert (concat "\n" infn-2)))
		(widen))))
	(setq infn (esn-rep-infn esn-cwres-infn5-advan-template 't))
	(let (
	      (theta.n (esn-max-theta))
	      (eta.n (esn-max-eta))
	      (eps.n (esn-max-eps))
	      (fn (buffer-file-name)))
	  (when (string-match "\\(DATA +NTH *, *NETA *, *NEPS *\\)/.*?/" infn)
	    (setq infn
		  (replace-match
		   (format "\\1/%s,%s,%s/" theta.n eta.n eps.n)
		   nil nil infn
		   )))
	  (when (and fn (string-match "\\..*?$" fn))
	    (setq fn (replace-match ".for" nil nil fn)))
	  (find-file fn)
	  (delete-region (point-min) (point-max))
	  (insert infn)
	  (save-buffer)
	  (kill-buffer (current-buffer))
	  (when (string-match "[/\\\\]\\([^/\\\\]*?\\)$" fn)
	    (setq fn (concat esn-completing-current-directory (match-string 1 fn))))
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward (eval-when-compile
                                         (esn-reg-record-exp "SUB")) nil t)
	      (esn-narrow-rec)
	      (goto-char (point-min))
	      (if (re-search-forward "\\<INFN? *= *[^ ]*" nil t)
		  (replace-match (format "INFN=%s" fn))
		(goto-char (point-max))
		(insert (format " INFN=%s" fn)))
	      (widen))))))
    (when (>= (string-to-number nm-ver) 6.0)
      (if (esn-rec "INF")
	  (progn
	    ;; Add to INFN block and/or replace template.
	    (esn-replace-in-record infn-re "" "INF")
	    (esn-insert-after (concat "\n" (esn-rep-infn infn)  "\n") (list "INF")))
	(if (esn-is-pred-p)
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward (eval-when-compile
                                         (esn-reg-record-exp "PRE")) nil t)
		(esn-narrow-rec)
		(goto-char (point-min))
		(when (re-search-forward (concat "\n?" infn-re "\n?") nil 't)
		  (replace-match ""))
		(goto-char (point-max))
		(re-search-backward "\" *LAST *$" nil t)
		(insert "\n")
		(insert  (esn-rep-infn infn))
		(widen)))
	  (if (esn-rec "PK")
	      (esn-insert-after (concat "\n\n$INFN\n" (esn-rep-infn infn) "\n") (list "PK")))))))))

(provide 'esn-cwres)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-cwres.el ends here
