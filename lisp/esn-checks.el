;;; esn-checks.el --- Check NONMEM control stream for specific things
;; 
;; Filename: esn-checks.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 16:36:10 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 27 19:11:21 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 2
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


(defun check-step-sizes ()
  "* Checks to make sure that the step sizes are correct."
  (interactive)
  (let (
	(sigl 0)
	(tol 0)
	(nsig 0)
	)
    (unless (<= sigl tol)
      (message "SIGL should be less than or equal to TOL.")
      )
    (unless (<= nsig (/ sigl 3))
      (message "NSIG should be less than or equal to SIGL/3")
      )
    )
  )
(defun check-mu-referencing ()
  ;; Rule 1:
  ;;
  ;; All right handed MU_i statments mut be of the form:
  ;;   (MU_i+ETA(i)) or = MU_i + ETA(i) or associtions thereof.
  ;;

  ;; Rule 2:
  ;;
  ;;  Once a THETA is defined in terms of MU_i, it cannot be used again.
  ;;
  ;; The only place THETA(x) may be used is when  MU_i+ETA(i) is described

  ;; Rule 3:
  ;;
  ;; When possible define thetas in terms of non-transformed values (performance
  ;; gains)

  ;; Rule 4:
  ;; Have all MU_ values defined in $ERROR

  ;; Rule 5:
  ;; Have MU_ values defined outside of IF THEN blocks

  ;; Rule 6:
  ;; Time dependent covarites cannot be part of the MU equation
  
  )
(provide 'esn-checks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-checks.el ends here
