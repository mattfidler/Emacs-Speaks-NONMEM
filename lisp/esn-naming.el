;;; esn-naming.el --- Naming convetion functions
;; 
;; Filename: esn-naming.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Mon May  2 12:40:56 2011 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 12:41:49 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 3
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
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

(declare-function esn-max-dadt "esn-properties")
(declare-function esn-rec "esn-properties")


;; Naming conventions for EsN

;; Absorption Types:
;; - First Order
;; - Zero Order
;; - Two parallel first-order
;; - First order and Zero order
;; - Weibull Absorption
;; - Enterohepatic Recycling

(defun esn-get-abs-type ()
  "Attempts to determine the type of absorption based on the file being opened."
  (interactive)
  (let (
	(advan (esn-advan))
	(abs nil)
	(tmp-s "")
	(consts '())
	(n '())
	(max-dadt (esn-max-dadt))
	(zero-ord '())
	(pk (esn-rec "PK" 't))
	)
    (when advan
      (if (or (= advan 2) (= advan 4) (= advan 12))
	  (setq abs "first")
	(when (or (= advan 6) )
	  (save-excursion
	    ;; Look for more first order types.
	    (goto-char (point-min))
	    ;; First search for differential equations with only an elimination
	    ;; Constant
	    (while (re-search-forward "^ *DADT(\\([0-9]\\)) *= *- *\\([A-Z][A-Z0-9]* *\\* *A(\\1)\\|A(\\1) *\\* *[A-Z][A-Z0-9]*\\) *$" nil t)
	      (add-to-list 'n (string-to-number (match-string 1)))
	      (setq tmp-s (match-string 2))
	      (while (string-match "A([0-9])" tmp-s)
		(setq tmp-s (replace-match "" nil nil tmp-s)))
	      (while (string-match " *\\* *" tmp-s)
		(setq tmp-s (replace-match "" nil nil tmp-s)))
	      (add-to-list 'consts tmp-s)
	      )
	    (when (> 0 (length n))
	      (message "Consts: %s, %s, %s, %s " consts n (apply 'max n) max-dadt))))))))
(provide 'esn-naming)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-naming.el ends here
