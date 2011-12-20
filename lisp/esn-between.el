;;; esn-between.el --- Provides the esn-is-between function
;; 
;; Filename: esn-between.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 10:32:21 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 27 19:11:23 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 4
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
;; 27-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Aug 27 10:32:35 2010 (-0500) #1 (Matthew L. Fidler)
;;    Added header.
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


(defun esn-is-between (first last &optional eof)
  "Returns true of the carat is between the first and last .

If eof is true, then the last position is:
   (1) the position of the variable last
   (2) the position of the next variable first
   (3) the end of the buffer.
If eof is nil, then the last position is:
   (1) the position of the next variable last.
"
  (let (
        (first-posB nil)
        (last-posF nil)
        (between 't)
        (case-fold-search 't))
    (save-excursion
      (if (re-search-backward first nil t)
          (setq first-posB (point))
        (setq between nil)))
    (if between
        (save-excursion
          (if (re-search-forward last nil t)
              (setq last-posF (point))
            ;; 
            ;; Last not found, look for first.
            ;;
            (if eof
                (progn
                  (if (re-search-forward first nil t)
                      (setq last-posF (point))
                    ;;
                    ;; First not found, set to end of buffer.
                    ;;
                    (setq last-posF (point-max))))
              ;;
              ;; Eof not true.  
              ;;
              (setq between nil)))))
    (if (and (not eof) between)
        (save-excursion
          (if (re-search-forward first nil t)
              (if (< (point) last-posF)
                  (setq between nil)))))
    (if between
        (save-excursion
          (if (re-search-backward last nil t)
              (if (> (point) first-posB)
                  (setq between nil)))))
    between))
(provide 'esn-between)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-between.el ends here
