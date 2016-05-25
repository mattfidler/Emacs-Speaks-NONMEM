;;; esn-tab-pred.el --- Gets the number of PRED generated items in a table
;; 
;; Filename: esn-tab-pred.el
;; Description:  Gets the number of PRED generated items in a table 
;;               (and perhaps splits the file)
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Thu Feb 11 10:52:20 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Mon May  2 14:08:16 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 36
;; URL: http://esnm.sourceforge.net
;; Keywords: http://esnm.sourceforge.net
;; Compatibility: Emacs 23.x
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
;;    Last-Updated: Thu Jan 20 10:12:01 2011 (-0600) #32 (Matthew L. Fidler)
;;    Changed ;| to ;@ to support orgtbl-comment-mode.
;; 20-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Aug 20 10:15:20 2010 (-0500) #18 (Matthew L. Fidler)
;;    Added Standard Regular expressions
;; 25-Jun-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Jun 25 09:01:41 2010 (-0500) #10 (Matthew L. Fidler)
;;    When updating number of output variables, don't count as a modification
;; 11-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Feb 11 10:53:44 2010 (-0600) #2 (Matthew L. Fidler)
;;
;;    Added header, changed counting of PRED generated items to a possible
;;    timer.
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

(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-xpose-get-input-line "esn-xpose")
(declare-function esn-get-current-record "esn-narrow")
(defun esn-gen-get-msfo ()
  "Get or generate msfo information.  First MSFO per problem is
returned.  MSFO is added to the first ESTIMATION or NONPARAMETRIC
record when not found."
  (save-excursion
    (let ((np  (esn-num-problems))
	  (msfo '())
	  (i 0)
	  (run (esn-runname-noext 't))
	  (tmp ""))
      (save-excursion
	(while (< i np)
	  (setq i (+ i 1))
	  (esn-narrow-to-problem i)
	  (goto-char (point-min))
	  (if (re-search-forward "\\<MSFO? *= *\\([^ \t\n]*\\)" nil t)
	      (setq msfo (push (match-string 1) msfo))
	    (goto-char (point-min))
	    (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp '("EST" "NON") nil 't)) nil t))
		(setq msfo (push "" msfo))
	      (setq tmp (match-string 1))
	      (esn-narrow-rec)
	      (setq tmp
		    (concat esn-completing-current-directory
			    (esn-mangle-problem-file-name
			     (concat run
				     (if (and (string= (downcase tmp) "est"))
					 esn-msfo-est
				       esn-msfo-non))
			     i)))
	      (insert (format " MSFO=%s " tmp))
	      (setq msfo (push tmp msfo))
	      (widen)
	      (widen)
	      (esn-narrow-to-problem i)))
	  (widen))
	(setq msfo (reverse msfo))))))

(defun esn-table-pred-count (&optional gen-reg)
  "* Counts the number of pred generated items in the table for
each $PROBLEM statment.  (AKA anything NOT in input). When genReg
is true, generates a regular expression of the PRED generated
items."
  (save-excursion
    (let ((tab "")
	  (inp (esn-xpose-get-input-line))
	  (tab-opts esn-table-pred-tab-opts)
	  (case-fold-search 't)
	  (count '())
	  (cc 0)
	  (cur-inp "")
	  (i 0)
	  (np 0)
	  (cur-reg '())
	  (tmp "")
	  (tlst '())
	  (max-cc 0)
	  (esn-table-pred-max-count (esn-update-get-version)))
      (when (string= "-1" esn-table-pred-max-count)
	(setq esn-table-pred-max-count esn-assumed-version))
      (if (>= (string-to-number esn-table-pred-max-count) 6.2)
	  (setq esn-table-pred-max-count 50)
	(setq esn-table-pred-max-count 20))
      (setq np (length inp))
      (save-excursion
	(while (< i np)
	  (setq cur-reg '())
	  (setq cur-inp (nth i inp))
	  (setq i (+ i 1))
	  (esn-narrow-to-problem i)
	  (setq tab (esn-rec "TAB" 't))
	  (widen)
	  (with-temp-buffer
	    (insert tab)
	    (goto-char (point-min))
	    (while (re-search-forward ";.*" nil t)
	      (replace-match ""))
	    (goto-char (point-min))
	    (while (re-search-forward tab-opts nil t)
	      (replace-match ""))
	    (goto-char (point-min))
	    (unless (string= cur-inp "")
	      (while (re-search-forward cur-inp nil t)
		(replace-match "")))
	    (goto-char (point-min))
	    (while (re-search-forward " *= *" nil t)
	      (replace-match "="))
	    (goto-char (point-min))
	    (setq tlst '())
	    ;; Take out doubles.
	    (while (re-search-forward "\\<.*?\\>" nil t)
	      (setq tmp nil)
	      (mapc (lambda(x)
		      (unless tmp
			(when (and x (string= (downcase x) (downcase (match-string 0))))
			  (setq tmp 't))))
		    tlst)
	      (when tmp
		(replace-match ""))
	      (add-to-list 'tlst (match-string 0)))
	    (setq tab (downcase (buffer-substring (point-min) (point-max)))))
	  (setq tab (split-string tab))
	  (setq cc (length tab))
	  (setq max-cc (max max-cc cc))

	  (if (not gen-reg)
	      (push cc count)
	    (setq tmp (regexp-opt tab 'words))
	    (setq tmp (format "%s\\(?:[ \t]*=[ \t]*%s\\)?" tmp tmp))
	    (push (list cc tmp) count)))
	(save-excursion
	  (when (and esn-table-split-pred (> max-cc esn-table-pred-max-count))
	    ;; Add MSFO if greater than esn-table-pred-max-count.
	    (esn-gen-get-msfo)))
	(setq count (reverse count))
	(symbol-value 'count)))))

;;;###autoload
(defun esn-table-split-count ()
  (when esn-table-split-count-timer
    (cancel-timer esn-table-split-count-timer))
  (setq esn-table-split-count-timer
	(run-with-timer 0.25 nil
			(lambda ()
			  (esn-table-split t)))))

(esn-rec-post-hook "table" 'esn-table-split-count)

(defun esn-table-split (&optional skip)
  "Splits the $TABLES & counts the number of output variables. When Skip=TRUE this doesn't count as a modification."
  (interactive)
  (save-excursion
    (let ((creg (esn-table-pred-count 't))
	  (i 0)
	  (count 0)
	  (comment "")
	  (tmp "")
	  (case-fold-search 't)
	  (max-file 0)
	  (files '())
	  (tmp-key "")
	  (current-var "")
	  (tmp-lst '())
	  (file-mod-lst '())
	  (msfo "")
	  (orig-file "")
	  (fn1 (buffer-file-name))
	  (fn2 "")
	  (esn-table-pred-max-count (esn-update-get-version))
	  (buf-mod (buffer-modified-p)))
      (when (string= "-1" esn-table-pred-max-count)
	(setq esn-table-pred-max-count esn-assumed-version))
      (if (>= (string-to-number esn-table-pred-max-count) 6.2)
	  (setq esn-table-pred-max-count 50)
	(setq esn-table-pred-max-count 20))

      (when (string-match "\\.[^.]*$" fn1)
	(setq fn2 (match-string 0 fn1))
	(setq fn1 (replace-match "" nil nil fn1)))
      (setq orig-file (buffer-substring (point-min) (point-max)))
      (mapc
       (lambda(x)
	 (let ((n (nth 0 x))
	       (reg (format "\\(?:[ \t]+\\|\\<\\)%s" (nth 1 x)))
	       (j 0)
	       (curr count)
	       (file 0)
	       (md nil)
	       (pt nil)
	       (options '())
	       (lfile 0))
	   (setq i (+ i 1))
	   (setq count (+ count n))
	   (when (and (not skip) (< esn-table-pred-max-count count))
	     (save-excursion
	       ;; Over the amount that NONMEM can handle,  split.
	       (esn-narrow-to-problem i)
	       (goto-char (point-min))
	       (while (re-search-forward reg nil 't)
		 (setq md (match-data))
		 (setq pt (- (point) (length (match-string 0))))
		 (save-excursion
		   (when (string= "TAB" (esn-get-current-record))
		     (setq curr (+ 1 curr))
		     (setq file (floor (/ (- curr 1) esn-table-pred-max-count)))
		     (setq max-file file)
		     (when (> file 0)
		       (set-match-data md)
		       (setq comment
			     (format "%s;@TBL:%s,%s@;\n" comment pt (match-string 0)))
		       (setq current-var (match-string 0))
		       (replace-match "")
		       (when (esn-narrow-rec)
			 (setq options '())
			 (save-excursion
			   (goto-char (point-min))
			   (while (re-search-forward (format "%s" esn-table-pred-tab-opts) nil t)
			     (setq options (push (match-string 0) options))))
			 (setq tmp-key (format "%s,%s,%s"
					       file
					       i
					       (mapconcat (lambda(x) x) options " ")))
			 (when (string-match "^[^ ]" current-var)
			   (setq current-var (concat " " current-var)))
			 (if (not (assoc tmp-key files))
			     (esn-add-to-alist 'files (list tmp-key current-var))
			   (esn-add-to-alist 'files (list tmp-key (concat (cadr (assoc tmp-key files)) current-var))))
			 (goto-char (point-min))
			 (re-search-forward (eval-when-compile (esn-reg-record-exp "TAB" 't)))
			 (while (re-search-forward
				 (format "\\=[ \t\n]+\\(%s\\(?:[ \t]*=[ \t]*[^ \t]+\\)?\\)"
					 esn-table-pred-tab-opts)
				 nil t)
			   )
			 (skip-chars-forward " \n\t")
			 (when (eobp)
			   (setq tmp (buffer-substring (point-min) (point-max)))
			   (while (string-match "\n" tmp)
			     (setq tmp (replace-match "\\\\n" nil nil tmp)))
			   (setq comment
				 (format
				  "%s;@TBL:%s,%s@;\n"
				  comment (point-min) tmp))
			   (delete-region (point-min) (point-max)))
			 (widen)
			 (esn-narrow-to-problem i))))))
	       (widen)))))
       creg)
      (when esn-table-pred-display
	(save-excursion
	  (let ((found-it nil))
	    (goto-char (point-max))
	    (while (re-search-backward (eval-when-compile (esn-reg-record-exp "TAB")) nil t)
	      (setq found-it t))
	    (when  found-it
	      (if (re-search-backward (concat "^ *;" (regexp-quote esn-sub-begin) " Pred Variables [0-9]+/[0-9]+ " (regexp-quote esn-sub-end) ";") nil 't)
		  (replace-match (format ";%s Pred Variables %s/%s %s;" esn-sub-begin count esn-table-pred-max-count esn-sub-end))
		(insert (format ";%s Pred Variables %s/%s %s;\n" esn-sub-begin count esn-table-pred-max-count esn-sub-end))
		)
	      )
	    )
	  )
	)
      (when (not skip)
	;; Make sure comment is less then 80 characters.
	(with-temp-buffer
	  (insert comment)
	  (goto-char (point-min))
	  (while (re-search-forward "^.\\{80\\}" nil t)
	    (unless (looking-at "$")
	      (insert "\n;@TBL80:")
	      (forward-line -1)
	      (beginning-of-line)))
	  (setq comment (buffer-substring (point-min) (point-max))))
	;; Now split up the tables
	(with-temp-buffer
	  (insert orig-file)
	  (goto-char (point-min))
	  ;; Remove $COVARIANCE $TABLE, $THETA, $SIGMA, and $OMEGA records.
	  (while (re-search-forward (eval-when-compile (esn-reg-record-exp (regexp-opt '("TAB" "THE" "SIG" "OME" "COV" "EST") 't) 't 't)) nil t)
	    (when (esn-narrow-rec)
	      (delete-region (point-min) (point-max))
	      (widen)))
	  (goto-char (point-min))
	  (setq msfo (esn-gen-get-msfo))
	  (mapc (lambda (msf)
		  (when (re-search-forward (format "= *%s" (regexp-quote msf)) nil t)
		    (when (esn-narrow-rec)
		      (delete-region (point-min) (point-max))
		      (widen)
		      (insert (concat "$MSFI " msf))
		      )
		    )
		  )
		msfo)
	  (setq orig-file (buffer-substring (point-min) (point-max)))
	  )
	(when (> (length files) 0)
	  (setq i 1)
	  (setq tmp-lst (all-completions (format "%s," i) files))
	  (while (> (length tmp-lst) 0)
	    (setq file-mod-lst
		  (mapcar (lambda(x)
			    (let (
				  (var (cadr (assoc x files)))
				  (var-lst (split-string x ","))
				  (problem -1)
				  (file -1)
				  (table "")
				  (lst '())
				  )
			      (setq table (nth 2 var-lst))
			      (setq file (nth 0 var-lst))
			      (setq problem (nth 1 var-lst))
			      (with-temp-buffer
				(insert table)
				(goto-char (point-min))
				(re-search-forward (eval-when-compile (format "\\=%s" (esn-reg-record-exp "TAB" 't))) nil t)
				(insert var)
				(goto-char (point-min))
				(while (re-search-forward "\\<NOAPPEND\\>" nil 't)
				  (replace-match ""))
				(goto-char (point-min))
				(while (re-search-forward "\\<FILE[ \t]*[= \t][ \t]*[^ \t]+" nil 't)
				  (insert (format "~%s NOAPPEND" file))
				  (let (
					(esn-skip-wrap nil)
					)
				    (esn-magic-wrap nil nil 't)
				    )
				  )
				(setq table (buffer-substring (point-min) (point-max)))
				)
			      (setq lst (list problem table))
			      (symbol-value 'lst)
			      )
			    )
			  tmp-lst
			  ))
	    (setq tmp-lst (all-completions (format "%s," i) files))
	    (when tmp-lst
	      (with-temp-buffer
		(insert orig-file)
		(mapc (lambda (x)
			(let (
			      (keys (split-string x ","))
			      (pt nil)
			      (pt2 nil)
			      (tmp "")
			      )
			  (goto-char (point-min))
			  (esn-narrow-to-problem (string-to-number (nth 1 keys)))
			  (goto-char (point-max))
			  (setq pt (point))
			  (insert "$TABLE ")
			  (setq tmp (nth 2 keys))
			  (while (string-match (eval-when-compile (esn-reg-record-exp "TAB" 't)) tmp)
			    (setq tmp (replace-match "" nil nil tmp)))
			  (insert tmp)
			  (insert "\n\n")
			  (goto-char pt)
			  (re-search-forward (eval-when-compile (format "\\=%s" (esn-reg-record-exp "TAB" 't))) nil t)
			  ;; Insert variables
			  (mapc (lambda (y)
				  (insert y)
				  )
				(cadr (assoc x files)))
			  (goto-char pt)
			  (end-of-line)
			  (esn-magic-wrap)
			  (widen)
			  )
			)
		      tmp-lst)
		(setq tmp (buffer-substring (point-min) (point-max)))
		(setq esn-mode-skip-ini 't)
		(find-file (format "%s~%s%s" fn1 i fn2))
		(when buffer-read-only
		  (setq buffer-read-only nil))
		(delete-region (point-min) (point-max))
		(insert tmp)
		(goto-char (point-min))
		(while (re-search-forward "\\<NOAPPEND\\>" nil t)
		  (replace-match ""))
		(goto-char (point-min))
		(while (re-search-forward "FILE[ \t]*[= \t][ \t]*[^ \t\n]*" nil t)
		  (when (string= "TAB" (esn-get-current-record))
		    (insert (format "~%s NOAPPEND" i))
		    (let (
			  (esn-skip-wrap nil)
			  )
		      (esn-magic-wrap nil nil 't)
		      )
		    )
		  )
		(save-buffer (current-buffer))
		(kill-buffer (current-buffer))

		(setq esn-mode-skip-ini nil)
		)
	      )
	    (setq i (+ i 1))
	    )
	  (save-excursion
	    (goto-char (point-min))
	    (while (re-search-forward "FILE[ \t]*[= \t][ \t]*[^ \t\n]*" nil t)
	      (when (string= "TAB" (esn-get-current-record))
		(insert "~0")
		)
	      )
	    )
	  (save-excursion
	    (goto-char (point-max))
	    (insert comment)
	    )
	  )
	)
      (unless buf-mod
	(when skip
	  (set-buffer-modified-p nil)
	  )
	)
      )
    ))
(defun esn-table-pred-undo-split ()
  (interactive)
  (let (
        (ob (current-buffer))
        (tb nil)
        (cmd "(setq tb (current-buffer)) (set-buffer ob)")
        )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "FILE[ \t]*[= \t][ \t]*[^ \t\n]*~0" nil t)
        (when (string= "TAB" (esn-get-current-record))
          (backward-char 2)
          (delete-char 2)
          )
        )
      )
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward "\n;@TBL80:" nil t)
        (replace-match "")
        )
      (goto-char (point-max))
      (while (re-search-backward "^;@TBL:\\([0-9]+\\),\\(.*?\\)@;$" nil t)
        (setq cmd
              (concat cmd
                      "(goto-char " (match-string 1) ")"
                      "(insert \"" (match-string 2) "\")"))
        (replace-match "")
        )
      (setq cmd (concat cmd "(goto-char (point-max)) (skip-chars-backward \"\\n\\t \")(end-of-line)(delete-region (point) (point-max)) (insert \"\n\")(set-buffer tb)" ))
      (with-temp-buffer
        (insert cmd)
        (eval-buffer)
        )
      )
    )
  (when esn-table-split-pt
    (goto-char esn-table-split-pt)
    )
  )

(provide 'esn-tab-pred)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-tab-pred.el ends here
