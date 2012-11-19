;;; esn-space.el --- Spacing routines for emacs speaks NONMEM
;; 
;; Filename: esn-space.el
;; Description: Spacing Routines for Emacs Speaks NONMEM
;; Author: Matthew L. Fidler
;; Maintainer: Matthew Fidler.
;; Created: Mon Feb  1 09:06:53 2010 (-0600)
;; Version: 
;; Last-Updated: Mon May  2 14:07:02 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 106
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
;; Compatibility: 23.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  Spacing Routines
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 20-Jan-2011    Matthew L. Fidler  
;;    Last-Updated: Thu Jan 20 10:32:49 2011 (-0600) #102 (Matthew L. Fidler)
;;    Updated subroutines comment.
;; 27-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Sep 27 17:18:49 2010 (-0500) #74 (Matthew L. Fidler)
;;    Changed esn-space-subroutines to use new options variable.
;; 14-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 14 13:33:02 2010 (-0500) #16 (Matthew L. Fidler)

;;    Added space subroutines bug fix when there are no records prior to
;;    $SUBROUTINES statement.

;; 12-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  9 22:10:43 2010 (-0600) #14 (Matthew L. Fidler)
;;    Removed Esn-space-input and put in esn-input
;; 01-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Feb  1 09:19:06 2010 (-0600) #8 (Matthew L. Fidler)
;;
;;    Took out extended highlighting when only outside of a comment.  Didn't
;;    affect the time-profile. (I.e. no marked improvement in responsiveness)
;;
;; 01-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Feb  1 09:08:11 2010 (-0600) #1 (Matthew L. Fidler)
;;    Made the numbering routine only update when outside of a comment. (Save
;;    time).
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
(declare-function esn-point-at-bor "esn-narrow")

(require 'esn-start)

;;
;; Provides NONMEM's spacing routines.
;;
(defun esn-fix-subroutine-text (txt)
  "Fixes the nonmem subroutine comment buffer."
  (let ((case-fold-search 't)
        (tmp txt))
    (while (string-match "^[ \t]*;;" tmp)
      (setq tmp (replace-match (concat ";" (regexp-quote esn-sub-begin)) nil nil tmp)))
    (while (string-match ";;[ \t]*$" tmp)
      (setq tmp (replace-match (concat (regexp-quote esn-sub-end) ";") nil nil tmp)))
    (symbol-value 'tmp)))

;;;###autoload
(defun esn-desc-subroutines (&rest ignored)
  "Adds descriptive text to subroutines ADVAN/TRANS combinations."
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
	(if (not esn-update-sub)
	    nil
	  (let ((advan (esn-advan))
		(trans (esn-trans))
		(desc "")
		(ret "")
		(tmp nil)
		(tmp2 nil)
		(pmn nil)
		(pmx nil)
		(rep nil)
		(rec (esn-get-current-rec)))
	    (when (string= rec "SUB")
	      (setq tmp (assoc advan esn-advan-trans-vars))
	      (when tmp
		(setq desc (nth (if esn-update-sub-small 1 2) tmp))
		(setq tmp2 (assoc trans (nth 4 tmp)))
		(if (not tmp2)
		    (progn
		      (format "TRANS%s not valid with ADVAN%s" trans advan)))
		(setq desc (concat desc "\n" (nth (if esn-update-sub-small 1 2) tmp2)))
		(cond
		 ( esn-update-sub-small
		   (with-temp-buffer
		     (insert desc)
		     (goto-char (point-min))
		     (while (not (eobp))
		       (insert (format ";%s "esn-sub-begin))
		       (goto-char (point-at-eol))
		       (insert (format "%s%s;" (make-string (- (- 80 (current-column)) 2) ? )
				       esn-sub-begin))
		       (skip-chars-forward "\n \t "))
		     (setq desc (buffer-substring (point-min) (point-max)))))
		 ( 't
		   (with-temp-buffer
		     (insert desc)
		     (goto-char (point-min))
		     (while (re-search-forward "^[ \t]*;;" nil t)
		       (replace-match (concat ";" esn-sub-begin) 't 't))
		     (goto-char (point-min))
		     
		     (while (re-search-forward ";;[ \t]*$" nil t)
		       (replace-match (concat esn-sub-begin ";") 't 't))
		     (setq desc (buffer-substring (point-min) (point-max)))))))
	      (when (and  (not (string= "" desc))
			  (not (string= "\n" (substring desc -1))))
		(setq desc (concat desc "\n")))
	      (when (string= "SUB" (esn-get-current-rec))
		;; Need to replace it.
		(save-excursion
		  (if (re-search-backward (format
					   "[ \t]*;%s.*%s;[ \t]*+[ \t\n]*"
					   (regexp-quote esn-sub-begin)
					   (regexp-quote esn-sub-end)) nil t)
		      (progn
			(replace-match "")
			(save-excursion
			  (skip-chars-backward " \t\n")
			  (goto-char (point-at-bol))
			  (while (looking-at (format
					      "[ \t]*;%s.*%s;[ \t]*+[ \t\n]*"
					      (regexp-quote esn-sub-begin)
					      (regexp-quote esn-sub-end)))
			    (skip-chars-backward " \t\n")
			    (goto-char (point-at-bol))))
			(when (looking-back (format "\\([ \t]*;%s.*%s;[ \t]*\n?\\)+[ \t\n]*"
                                                    (regexp-quote esn-sub-begin)
                                                    (regexp-quote esn-sub-end)) nil t)
			  (delete-region (match-beginning 0) (match-end 0)))
			(insert desc))
		    (goto-char (esn-point-at-bor))
		    (insert desc)))))))))))
(esn-rec-post-hook "subroutines" 'esn-desc-subroutines)

(require 'esn-indent)
(provide 'esn-space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-space.el ends here
