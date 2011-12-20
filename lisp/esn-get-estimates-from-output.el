;;; esn-get-estimates-from-output.el --- Get estimates from output
;; 
;; Filename: esn-get-estimates-from-output.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 16:19:36 2010 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 11:55:17 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 8
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; NEEDS to be updated for PLT tools and NONMEM 7.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
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

(declare-function esn-number-est "esn-align")
(defun esn-make-seq (lst &optional start)
  "
When lst is a list:
  Makes a sequence starting with \"start\" and with the same
  length of the list \"lst\" and return it in an list
When lst is an integer:
  When only using one argument it creates a list from 0 to argument 1
  When using a second argument cretes a sequence from arguemnt 1 to arugument 2

"
  
  (let ((i (or start 0))
	(j nil)
 	(seq nil))
    (when (sequencep lst)
      (setq seq (mapcar (lambda(x)
			  (let (
				(ret i))
			    (setq i (+ i 1))
			    (symbol-value 'ret)))
			lst)))
    (when (integerp lst)
      (if (= i 0)
	  (setq j lst)
	(setq j i)
	(setq i lst))
      (while (>= j i)
	(setq seq (push j seq))
	(setq j (- j 1))))
    (symbol-value 'seq)))

(defun esn-parse-matrix (mat)
  "Parses and returns a matrix-type object"
  (let (
	(mat-name nil)
	(mat-val nil)
	(seq nil)
	(tlst nil)
	(last-line "")
	(i 0))
    (with-temp-buffer
      (insert mat)
      (goto-char (point-min))
      (when (re-search-forward "\\(TH ?\\|OM\\|SG\\|ETA\\|EPS\\)[0-9]+" nil t)
	(beginning-of-line)
	(when (looking-at ".*")
	  (setq last-line (match-string 0))
	  (replace-match "")
	  (forward-line 1)
	  (beginning-of-line)
	  (while (looking-at "^[ \t]$")
	    (forward-line 1)
	    (beginning-of-line))
	  (setq mat-name "")
	  (while (looking-at ".*\\(TH ?\\|OM\\|SG\\|ETA\\|EPS\\)[0-9]+.*$")
	    (setq mat-name (concat mat-name " " last-line))
	    (setq last-line (match-string 0))
	    (replace-match "")
	    (forward-line 1)
	    (beginning-of-line)
	    (while (looking-at "^[ \t]$")
	      (forward-line 1)
	      (beginning-of-line))))
	(while (string-match "TH +" mat-name)
	  (setq mat-name (replace-match "TH" 't nil mat-name)))
	(setq mat-name (split-string mat-name))
	(goto-char (point-min))
	(while (re-search-forward "\\(TH ?\\|OM\\|SG\\|ETA\\|EPS\\)[0-9]+" nil t)
	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "^\\+ *" nil t)
	  (replace-match ""))
	(goto-char (point-min))
	(while (re-search-forward "\n" nil t)
	  (replace-match " "))
	(goto-char (point-min))
	(while (re-search-forward "  +"  nil t)
	  (replace-match " "))
	(setq mat-val
	      (mapcar (lambda(x)
			(number-to-string (string-to-number x)))
		      (split-string (buffer-substring (point-min) (point-max)))))
	(setq seq (esn-make-seq mat-name))
	(setq i 0)
	(setq tlst '())
	(mapc
	 (lambda(x)
	   (let (
		 (seq0 (esn-make-seq x)))
	     (mapc (lambda(y)
		     (setq tlst (push 
				 (list (format "%s,%s" (nth y mat-name) (nth x mat-name))
				       (nth i mat-val)
				       )
				 tlst))
		     (setq i (+ i 1)))
		   seq0)))
	 seq)
	(setq mat-val (reverse tlst))))
    (symbol-value 'mat-val)))
(defun esn-get-estimates-from-current-output-buffer (&optional reg-type)
  "Gets the estimates from the current buffer."
  (let (
	(pt1 nil)
	(pt2 nil)
	(fpe-txt nil)
	(th-txt nil)
	(th-num "")
	(th-val nil)
	(seq nil)
	(om-txt nil)
	(om-val nil)
	(sg-txt nil)
	(sg-val nil)
	(ret nil)
        p1
        i
        omega-val
	(reg (or reg-type "^ *[*]+ *FINAL PARAMETER ESTIMATE.*")))
    (goto-char (point-min))
    (when (re-search-forward reg  nil t)
      (end-of-line)
      (setq pt1 (point))
      (if (not (re-search-forward "^1" nil t))
	  (setq pt2 (point-max))
	(beginning-of-line)
	(setq pt2 (point)))
      (setq fpe-txt (buffer-substring pt1 pt2))
      (with-temp-buffer
	(insert fpe-txt)
	(goto-char (point-min))
	(when (re-search-forward "THETA.*VECTOR" nil t)
	  (end-of-line)
	  (setq p1 (point))
	  (when (re-search-forward "OMEGA.*MATRIX" nil t)
	    (beginning-of-line)
	    (setq th-txt (buffer-substring p1 (point))))))
      (when th-txt
	(with-temp-buffer
	  (insert th-txt)
	  (goto-char (point-min))
	  (setq th-num "")
	  (while (re-search-forward ".*TH.*" nil t)
	    (setq th-num (format "%s %s" th-num (match-string 0)))
	    (replace-match ""))
	  (goto-char (point-min))
	  (while (re-search-forward "\n" nil t)
	    (replace-match " "))
	  (goto-char (point-min))
	  (while (re-search-forward "  +" nil t)
	    (replace-match " "))
	  (setq th-val
		(mapcar
		 (lambda(x)
		   (string-to-number x)
		   )
		 (split-string
		  (buffer-substring (point-min) (point-max)))))
	  (while (string-match "TH +" th-num)
	    (setq th-num (replace-match "TH" nil nil th-num)))
	  (setq th-num (split-string th-num))
	  (setq i 0)
	  (setq seq (esn-make-seq th-num))
	  (if (= (length th-val) (length th-num))
	      (setq th-val (mapcar (lambda(x)
				     (list (nth x th-num) (nth x th-val))
				     )
				   seq))
	    (setq th-val nil))))
      (when th-val
	(with-temp-buffer
	  (insert fpe-txt)
	  (goto-char (point-min))
	  (when (re-search-forward "OMEGA.*MATRIX.*" nil t)
	    (end-of-line)
	    (setq pt1 (point))
	    (when (re-search-forward "SIGMA.*MATRIX" nil t)
	      (beginning-of-line)
	      (setq om-txt (buffer-substring pt1 (point))))))
	(setq omega-val (esn-parse-matrix om-txt)))
      (when omega-val
	(with-temp-buffer
	  (insert fpe-txt)
	  (goto-char (point-min))
	  (when (re-search-forward "SIGMA.*MATRIX" nil t)
	    (end-of-line)
	    (setq pt1 (point))
	    (goto-char (point-max))
	    (when (re-search-backward "^[ \t]*\\+" nil t)
	      (end-of-line)
	      (setq sg-val (esn-parse-matrix (buffer-substring pt1 (point))))
	      (setq ret (append th-val omega-val sg-val)))))))
    (symbol-value 'ret)))

(defun esn-get-estimates-from-output ()
  "Gets estimates from the output buffer associated with this file."
  (interactive)
  (let (
	(of (esn-get-nonmem-output))
	(fpe '())
	(buffer-undo-list 't))
    (when of
      (with-temp-buffer
	(insert-file-contents of)
	(setq fpe (esn-get-estimates-from-current-output-buffer))))
    (symbol-value 'fpe)))
(defun esn-update-est-based-on-output ()
  "Updates the estimates of the current file based on the NONMEM listing."
  (interactive)
  (let (
	(fpe (esn-get-estimates-from-output))
	(theta-count 1)
	(num "\\<[+-]?[0-9.]+\\(?:[Ee][+-][0-9]+\\)?\\>")
	(val 0))
    (esn-number-est "THE" "TH" fpe)
    (esn-number-est "OME" "ETA" fpe)
    (esn-number-est "SIG" "EPS" fpe)))


(provide 'esn-get-estimates-from-output)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-get-estimates-from-output.el ends here
