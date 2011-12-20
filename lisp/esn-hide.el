;;; esn-hide.el --- Hiding portions of the Control stream
;; 
;; Filename: esn-hide.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 16:18:17 2010 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 12:25:24 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 10
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

(declare-function esn-header-regexp-quote "esn-update")
(declare-function esn-narrow-to-header "esn-update")
(declare-function esn-is-foce-p "esn-properties")
(declare-function esn-is-focei-p "esn-properties")
(declare-function esn-is-hybrid-p "esn-properties")
(declare-function esn-is-fo-posthoc-p "esn-properties")
(declare-function esn-is-foi-posthoc-p "esn-properties")
(declare-function esn-quote-infn "esn-cwres")
(declare-function esn-rep-infn "esn-cwres")

; Provides hiding ability to headers so that user does not alter automatically generated content.
; Based on hide-lines by Mark Hulme-Jones.
; Also adds read-only property to the buffer.
(add-to-invisibility-spec 'esn-hl)
(defun esn-add-invisibile-overlay (start end)
  "Add an overlay from `start' to `end' in the current buffer.  Push the overlay onto the esn-invisible-areas-list."
  (let (
	(case-fold-search 't)
	(ret '())
	(bm (buffer-modified-p)))
    (if esn-hide-uneditable-header-components
	(let (
	      (case-fold-search 't)
	      (overlay (make-overlay start end)))
	  (setq esn-invisible-areas-list (cons overlay esn-invisible-areas-list))
	  (if (not esn-hide-uneditable-header-components) nil
	    (overlay-put overlay 'invisible 'esn-hl))))
    ;; Now read only
    (let (
	  (case-fold-search 't)
	  (inhibit-read-only 't))
      (save-excursion
	(goto-char start)
	(if (re-search-backward "\n\\=" nil t)
	    (setq start (point)))
	(goto-char end)
	(unless (eobp) 
	  (beginning-of-line)
	  (setq end (point)))
	;; Add Space.
	(when (looking-at "\\(\n? *;+ *.*?:\\)$")
	  (replace-match (concat (match-string 1) " ")))
	(if (looking-at "\n? *;+ *.*?:")
	    (progn
	      (when esn-set-read-only
		(put-text-property start
				   (+ (length (match-string 0)) end)
				   'read-only
				   "Cannot manually edit updatable portion of header."
				   ))
	      (setq ret (list start (+ (length (match-string 0)) end))))
	  (when esn-set-read-only
	    (put-text-property
	     start end 'read-only
	     "Cannot manually edit updatable portion of header."))
	  (setq ret (list start end)))))
    (set-buffer-modified-p bm)
    (symbol-value 'ret)))
;;;###autoload
(defun esn-hide-header ()
  "Hides non-editable portions of the header.  Makes editable
poritions of buffer highlighted."
  (interactive)
  (save-restriction
  (esn-rwt-all-buffer)
  (esn-show-all-invisible)  
  (let (
	(case-fold-search 't)
	(header-regexp (esn-header-regexp-quote))
	(start-position nil)
	(point2 nil)
	(inhibit-read-only 't)
	(tmp nil)
	(intang '())
	(last nil)
	(bm (buffer-modified-p))
	(reg (regexp-opt (mapcar (lambda(x) (nth 0 x)) (esn-update-editable-regions))))
	(next "")
	(tmp2 nil))
    (if (and esn-set-read-only esn-hide-the-header)
	(save-excursion
	  (when (esn-narrow-to-header)
	    (goto-char (point-min))
	    (setq start-position (point))
            (while (re-search-forward ":$" nil t)
              (insert " "))
            (goto-char (point-min))
	    (while (re-search-forward reg nil t)
	      (setq point2 (point))
	      (setq next (match-string 0))
	      (backward-char (length next))
	      (setq tmp (esn-add-invisibile-overlay start-position (point)))
	      (push tmp intang)
	      (goto-char point2)
	      (end-of-line)
	      (if (search-forward (nth 1 (assoc next (esn-update-editable-regions))) nil t)
		  (progn
		    (backward-char (length (match-string 0)))
		    (beginning-of-line))
		(skip-chars-forward "\n"))
	      (beginning-of-line)
	      (setq start-position (point)))
	    (push (esn-add-invisibile-overlay start-position (point-max)) intang)
	    (widen))))
    ;; Hide Code
    (if (and
	 esn-set-read-only
	 esn-hide-cwres-generated-code
	 (or
	  (and esn-cwres-foce (esn-is-foce-p))
	  (and esn-cwres-focei (esn-is-focei-p))
	  (and esn-cwres-hybrid (esn-is-hybrid-p))
	  (and esn-cwres-fo-posthoc (esn-is-fo-posthoc-p))
	  (and esn-cwres-foi-posthoc (esn-is-foi-posthoc-p))))
      (save-excursion
	(goto-char (point-min))
	(let (
	      (case-fold-search 't)
	      (infn-re (esn-quote-infn esn-cwres-infn-template)))
	  (if (not (re-search-forward infn-re nil t)) nil
	    (push (esn-add-invisibile-overlay  (- (point) (length (match-string 0)))
					       (point)) intang)))
	(goto-char (point-min))
	(while (re-search-forward "^\" +COM([0-9]+) *= *\\(G\\|H\\)H?([0-9]+ *, *1).*\n?" nil t)
	  (push (esn-add-invisibile-overlay  (- (point) (length (match-string 0)))
					     (point)) intang))
	(let (
	      (infn-1 esn-cwres-infn5-pred-template-1)
	      (infn-2 (esn-rep-infn esn-cwres-infn5-pred-template-2 't)))
	    (while (string-match "[ \n\t]" (substring infn-1 -1))
	      (setq infn-1 (substring infn-1 0 -1)))
	    (while (string-match "[ \n\t]" (substring infn-1 0 1))
	      (setq infn-1 (substring infn-1 1)))
	    (while (string-match "\" *\\(FIRST\\|LAST\\) *\n?" infn-1)
	      (setq infn-1 (replace-match "" nil nil infn-1)))

	    (while (string-match "[ \n\t]" (substring infn-2 -1))
	      (setq infn-2 (substring infn-2 0 -1)))
	    (while (string-match "[ \n\t]" (substring infn-2 0 1))
	      (setq infn-2 (substring infn-2 1)))
	    (while (string-match "\" *\\(FIRST\\|LAST\\) *\n?" infn-2)
	      (setq infn-2 (replace-match "" nil nil infn-2)))
	    
	    (goto-char (point-min))
	    (while (re-search-forward (format "%s\n?" (esn-quote-infn infn-1)) nil t)
	      (push (esn-add-invisibile-overlay (- (point) (length (match-string 0))) (point)) intang))
	    (goto-char (point-min))
	    (while (re-search-forward (format "%s\n?" (esn-quote-infn infn-2)) nil t)
	      (push (esn-add-invisibile-overlay (- (point) (length (match-string 0))) (point)) intang)))))
    ;; Now make all the read-only regions intangible.
    (while (< 0 (length intang))
      (setq tmp (pop intang))
      (when tmp
	(put-text-property (nth 0 tmp) (nth 1 tmp)  'intangible "")
	(setq last (nth 0 tmp))))
    (set-buffer-modified-p bm))))
(defun esn-tangible-all-buffer ()
  "Make all of the buffer tangible."
  (interactive)
  (let (
	(case-fold-search 't)
	(inhibit-read-only 't)
	(bm (buffer-modified-p)))
    (remove-text-properties (point-min) (point-max) (list 'intangible))
    (set-buffer-modified-p bm)))
(defun esn-rwt-all-buffer ()
  "Make all of the buffer read-write/tangible."
  (interactive)
  (let (
	(case-fold-search 't)
	(inhibit-read-only 't))
    (esn-tangible-all-buffer)
    (esn-rw-all-buffer)
    (esn-show-all-invisible)))
(defun esn-rw-all-buffer ()
  "Make all of the buffer read-write."
  (interactive)
  (let (
	(case-fold-search 't)
  	(inhibit-read-only 't)
	(bm (buffer-modified-p)))
    (remove-text-properties (point-min) (point-max) (list 'read-only))
    (set-buffer-modified-p bm)))

(defun esn-show-all-invisible ()
  "Show all areas hidden by the filter-buffer command"
  (interactive)
  (let (
	(case-fold-search 't)
	(bm (buffer-modified-p)))
    (mapc (lambda (overlay) (delete-overlay overlay)) 
	    esn-invisible-areas-list)
    (setq esn-invisible-areas-list ())
    (set-buffer-modified-p bm)))
(provide 'esn-hide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-hide.el ends here
