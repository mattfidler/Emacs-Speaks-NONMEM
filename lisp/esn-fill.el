;;; esn-fill.el --- Fill/Word Wrap routines
;; 
;; Filename: esn-fill.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 16:27:51 2010 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 10:33:25 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 24
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
;; 15-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep 15 09:20:34 2010 (-0500) #9 (Matthew L. Fidler)
;;    On wrap take off any symbol representation then put it back on.
;; 14-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 14 10:48:46 2010 (-0500) #7 (Matthew L. Fidler)
;;
;;    Added a decompose region while wrapping so that pretty printing doesnt
;;    affect wrapping.
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

(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-compose-region "esn-fontlock")

(defun esn-dewrap-narrowed-record (len)
  "This unwraps a narrowed region for later wrapping. It returns a skip variable
Skip=1 skip forward
Skip=2 skip backward
Skil=nil no skip."
  (let* (
	 (case-fold-search 't)
	 (cp (point))
	 (sp (make-string len ? ))
	 (eol (concat "\n" sp))
	 (is-continuation nil)
	 (what "")
	 (pt1 nil)
	 (pt2 nil)
	 (ptR nil)
	 (ptTmp nil)
	 (tmp "")
	 (comment-eol nil)
	 (at-comment nil)
	 (skip nil)
	 (only-one nil)
	 (on-line nil)
	 (md nil)
	 (cont-len nil))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward "[^ \n]\\([ \n][ \n]+\\|\n\\)" nil t)
	(setq what (match-string 1))
	(forward-char 1)
	(setq pt1 (point))
	(save-excursion
	  (setq comment-eol (re-search-backward ";[^;\n]*\\=" nil t)))
	(forward-char (length what))
	(setq at-comment (looking-at ";"))
	(setq pt2 (point))
	(setq only-one nil)
	(save-excursion
	  (if (not (looking-at ".*;.*")) nil
	    (re-search-forward ";" nil t)
	    (skip-chars-backward "; C")
	    (esn-backward-w)
	    (setq only-one (re-search-backward "\n *\\=" nil t)))
	  (goto-char pt2)
	  (beginning-of-line)
	  (setq ptTmp (point)) 
	  (goto-char cp)
	  (beginning-of-line)
	  (setq on-line nil)
	  (if (= (point) ptTmp)
	      (progn
		(goto-char cp)
		(esn-backward-w)
		(setq on-line (re-search-backward "\n *\\=" nil t)))))
	(if (and (<= pt1 cp) (<= cp pt2)) 
	      (progn
		(if (not (string-match "\n" what))
		    (goto-char pt1) ; No spacing for current point.
		  ;; Fix spacing for other line.
		  (goto-char pt1)
		  (end-of-line)
		  (setq ptR (point))
		  (if (and (<= pt1 cp) (<= cp ptR)) ; point on first line.
		      (progn
			(save-excursion
			  (beginning-of-line)
			  (if (looking-at "^\\(.*?\\);")
			      (setq cont-len (length (match-string 1)))
			    (setq cont-len 0))
			  (end-of-line)
			  (when (> cont-len  0)
			    (when (looking-at (format "\n \\{%s\\};\\(?:[|!C%s]\\)? *" cont-len esn-sub-begin))
			      (unless (looking-at (format "\n \\{%s\\};[;C] *" cont-len))
				(looking-at (format "\n \\{%s\\};\\(?:%s\\|!\\|C\\)? *" cont-len esn-sub-begin))
				(setq md (match-data))
				(when (string-match (format ";%s" esn-sub-begin) (match-string 0))
				  ;; Remove last |;
				  (when (re-search-backward
					 (format "%s; *\n?\\=" esn-sub-end))
				    (replace-match "")))
				(set-match-data md)
				(replace-match " ")))))
			(goto-char pt1))
		    ;; Try to fix comments for the keystrokes space/backspace if you press a space on a comment line only.
		    (setq is-continuation nil)
		    (save-excursion
		      (skip-chars-forward " \t\n")
		      (beginning-of-line)
		      (if (not (looking-at "\\( +\\);")) nil
			(setq tmp (match-string 1))
			(setq cont-len (length tmp))
			(forward-line -1)
			(beginning-of-line)
			(while (looking-at (format "\\( \\{%s,%s\\}\\);"
						   (max 0 (- cont-len 1))
						   (+ cont-len 1)))
			  (if (looking-at (format "\\( \\{%s,%s\\}\\);[;C]"
						  (max 0 (- cont-len 1))
						  (+ cont-len 1)))
			      nil
			    (looking-at (format "\\\( \\{%s,%s\\}\\);"
						(max 0 (- cont-len 1))
						(+ cont-len 1)))
			    (replace-match (concat (make-string cont-len ? ) ";")))
			  (forward-line -1)
			  (beginning-of-line))
			(if (not (looking-at (format "\\(.\\{%s,%s\\}\\);"
						     (max 0 (- cont-len 1))
						     (+ cont-len 1)))) nil
			  (if (looking-at (format "\\(.\\{%s,%s\\}\\);[;C]"
						  (max 0 (- cont-len 1))
						  (+ cont-len 1))) nil
			    (looking-at (format "\\(.\\{%s,%s\\}\\);"
						(max 0 (- cont-len 1))
						(+ cont-len 1)))
			    (setq tmp (match-string 1))
			    (if (> cont-len  (length tmp))
				(progn
				  (replace-match (concat "\\1" (make-string (- cont-len (length tmp)) ? ) ";"))
				  (setq is-continuation 't)
				  (setq skip 1))
			      (if (< cont-len (length tmp))
				  (progn
				    (if (not (looking-at "\\(.*?\\)\\( +\\);")) nil
				      (replace-match (concat "\\1" (make-string (- (length (match-string 2)) 1) ? ) ";"))
				      (setq is-continuation 't)
				      (setq skip 2)))))))))
		    (skip-chars-backward " \n")
		    (if (not (and is-continuation (looking-at "[ \n]*; *"))) nil
		      (progn 
			(if (looking-at "[ \n]*;[;C]") nil
			  (looking-at "[ \n]*;")
			  (replace-match " "))))
		    (setq is-continuation nil))))
	    (goto-char cp)
	    (save-excursion
	      (goto-char pt1)
	      (if (not (string-match "\n" what))
		  (progn
		    (if at-comment nil
		      (if (looking-at "[ \n]+")
			  (replace-match " "))))
		;; Returns.
		(if (and at-comment comment-eol)
		    (progn
		      (if (looking-at "[ \n]+;[ \n]*") ; Continuation comment. 
			  (progn
			    (setq is-continuation nil)
			    (save-excursion
			      (beginning-of-line)
			      (if (not (looking-at "\\(.+?\\)\\( +[^ ]+ *\\)?;")) nil
				(forward-line 1)
				(beginning-of-line)
				(if (not (looking-at (format " \\{%s,%s\\};" 
							     (- (length (match-string 1)) 1)
							     (+ (length (match-string 1)) 
								(+ 1 (length (match-string 2)))))))  nil
				  (setq is-continuation 't))))
			    (if (not (and is-continuation (looking-at "[ \n]+;[ \n]*"))) nil
			      (if (looking-at "[ \n]+;[;C][ \n]*") nil
				(looking-at "[ \n]+;[ \n]*")
				(replace-match " ")
				(skip-chars-backward " \n"))))))
		  (if comment-eol
		      (progn
			(if (looking-at "[ \n]+") ;End of comment.
			    (replace-match eol)))
		    (if at-comment 
			(progn
			  )
		      ;; Replace with space.
		      (if (looking-at "[ \n]+")
			  (if (or only-one on-line)
			      (replace-match eol)
			    (replace-match " ")))))))
	      (setq pt2 (point)))
	    (setq cp (point))
	    (goto-char pt1)
	    (skip-chars-backward "[ \t\n]+"))))
    (goto-char cp)
    (symbol-value 'skip)))

(defun esn-wrap-narrowed-record (len)
  "This function finishes wraping the record after everything has been ``dewraped''"
  (let* (
	 (case-fold-search 't)
	 (sp (make-string len ? ))
	 (eol (concat "\n" sp))
	 (lngth 0)
	 (whats-left "")
	 (comment-eol nil)
	 (insert-a-new-line nil)
	 (tmp-pt nil)
	 (maxed-comment nil)
	 (comment-sp)
	 (bol nil)
	 (l1 nil)
	 (l2 nil)
	 (semi ";"))
    (save-excursion
      (goto-char (point-min))
      (looking-at "^.*$")
      (setq lngth (length (match-string 0)))
      (save-excursion
	(if (re-search-forward "\\(\\(?:.\\|\n\\)*\\)" nil t)
	    (setq whats-left (match-string 0))))
      (while (string-match "[^ \n\t]" whats-left)
	(while (<= esn-character-limit lngth)
	  (forward-char esn-character-limit)
	  (esn-backward-w)
	  (save-excursion
	    (setq bol (re-search-backward "^ *\\=" nil t)))
	  (if (not bol) nil
	    (esn-forward-w)
	    (skip-chars-forward " "))
	  (save-excursion
	    (setq comment-eol (re-search-backward ";[^;\n]*\\=" nil t)))
	  (if (not comment-eol)
	      (progn
		(save-excursion
		  (if (re-search-backward " +\\=" nil t)
		      (replace-match "")))
		
		(insert eol)
		(if bol
		    (save-excursion
		      (forward-line -1)
		      (beginning-of-line)
		      (if (not (looking-at "^\\( *\\)\\(.*?\\) *$")) nil
			(setq l1 (length (match-string 1)))
			(setq l2 (length (match-string 2)))
			(if (> (- esn-character-limit l2)  0)
			    (if (looking-at "^ *")
				(replace-match (make-string (- esn-character-limit l2) ? )))
			  (if (looking-at "^ *")
			      (replace-match "")))))))
	    (setq insert-a-new-line t)
	    (save-excursion
	      (beginning-of-line)
	      (when (looking-at
		     (format "^.*?\\(;[;C]*\\(?:%s\\|!\\)?\\)" esn-sub-begin
			     ))
		(setq semi (match-string 1))
		(if (>= 60 (length (match-string 0))) nil
		  (forward-char (length (match-string 0)))
		  (setq tmp-pt (point))
		  (esn-backward-w (format " ;!%s" esn-sub-begin))
		  (save-excursion
		    (if (re-search-backward "\n\\( *\\)\\=" nil t)
			(if (= (length (match-string 1)) (length sp))
			    (setq maxed-comment 't))))
		  (if (not maxed-comment)
		      (insert (concat eol ))
		    (goto-char tmp-pt)
		    (skip-chars-backward (format " \n;%s!" esn-sub-begin))
		    (when (re-search-forward (format "[ \n;!%s]*" esn-sub-begin) nil t)
		      (replace-match (concat eol semi " "))))
		  (setq insert-a-new-line nil))))
	    (if (not insert-a-new-line) 
		(progn
		  (beginning-of-line)
		  (skip-chars-backward " \t\n"))
	      (insert "\n")
	      (save-excursion
		(forward-line -1)
		(beginning-of-line)
		(if (looking-at (format "^\\(.*?\\)\\(;[;C]*\\(?:%s\\|!\\)?\\)\\( *\\)" esn-sub-begin))
		    (progn
		      (setq comment-sp (concat (make-string (length (match-string 1)) ? ) (match-string 2) (make-string (length (match-string 3)) ? ))))
		  (setq comment-sp semi)))
	      (when (string-match (format ";%s" esn-sub-begin) comment-sp)
		(save-excursion
		  (forward-line -1)
		  (end-of-line)
		  (insert (format "%s;" esn-sub-end))))
	      (insert comment-sp)))
	  (beginning-of-line)
	  (looking-at "^.*$")
	  (setq lngth (length (match-string 0))))
	(forward-line 1)
	(save-excursion
	  (if (re-search-forward "\\(\\(?:.\\|\n\\)*\\)" nil t)
	      (setq whats-left (match-string 0))))
	(if (looking-at "^.*$")
	    (setq lngth (length (match-string 0)))
	  (end-of-line)
	  (setq lngth 0)))
      (goto-char (point-min)))))
;;;###autoload
(defun esn-fill-record (&optional r)
  (interactive)
  (save-restriction
  (let (
	(case-fold-search 't)
	(len 0)
	(updated 't)
	(rec (or r (esn-get-current-record)))
	(cont-len 0)
	(skip nil)
	(al nil)
	(num 0))
    (save-excursion
      (setq updated (esn-narrow-rec))
      (save-restriction
        (when esn-use-symbols
          (decompose-region (point-min) (point-max)))
        (if (not updated) nil
          ;; Turn off indenting if esn-mode-auto-indent is off.
          (setq len updated)
          (if (not esn-mode-auto-indent)
              (setq len 0))
          (if (string-match (concat "\\(" (regexp-opt esn-mode-auto-indent-force-zero-indent)
                                    "\\)") rec)
              (setq len 0))
          (setq skip (esn-dewrap-narrowed-record len))
          (save-excursion
            (goto-char (point-max))
            (skip-chars-backward " \t\n")
            (skip-chars-forward " \t")
            (if (looking-at "[ \t\n]*")
                (replace-match "")))
          (esn-wrap-narrowed-record len))
        (cond
         ((string= rec "MSF") ; Hack for MSFI spacing.  Make SURE the
                              ; file name is on the same line.  It
                              ; won't work otherwise.
          (goto-char (point-min))
          (when (re-search-forward (eval-when-compile (esn-reg-record-exp "MSF")) nil t)
            
            (delete-region (point) (progn (skip-chars-forward " \t\n") (point)))
            (insert " "))))
        (when esn-use-symbols
          (esn-compose-region (point-min) (point-max)))
        ))
    (if (not (and skip (= skip 2))) nil 
      (end-of-line))
    (if (not skip) nil
      (skip-chars-forward " \n")
      (beginning-of-line)
      (skip-chars-forward " \n"))
    ; Hack for BLOCK(#) spacing.
    (if (not (re-search-backward "BLOCK *\\(( *[0-9]+ *)\\)?\\=" nil t)) nil
      (re-search-forward "\\=BLOCK *\\(( *[0-9]+ *)\\)? ?" nil t))

    (symbol-value 'updated))))


(provide 'esn-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-fill.el ends here
