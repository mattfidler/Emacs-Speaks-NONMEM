;;; esn-coding.el --- Change file-coding based on options
;; 
;; Filename: esn-coding.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 27 16:35:13 2010 (-0500)
;; Version: 
;; Last-Updated: Wed Apr 27 19:11:20 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 6
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


(defun esn-unix ()
  "Change the current buffer to Latin 1 with Unix line-ends"
  (interactive)
  (set-buffer-file-coding-system
   (coding-system-change-eol-conversion
    (cond ((null buffer-file-coding-system) 'undecided)
          ((eq buffer-file-coding-system 'no-conversion) 'raw-text)
          (buffer-file-coding-system)) 0)))

(defun esn-dos ()
  (interactive)
  (set-buffer-file-coding-system
   (coding-system-change-eol-conversion
    (cond ((null buffer-file-coding-system) 'undecided)
          ((eq buffer-file-coding-system 'no-conversion) 'raw-text)
          (buffer-file-coding-system)) 1) t))

(defun esn-mac ()
  (interactive)
  (set-buffer-file-coding-system
   (coding-system-change-eol-conversion
    (cond ((null buffer-file-coding-system) 'undecided)
          ((eq buffer-file-coding-system 'no-conversion) 'raw-text)
          (buffer-file-coding-system)) 2) t))

;;;###autoload
(defun esn-file-coding-system ()
  "* Function that changes to the appropriate file system."
  (interactive)
  (cond
   ( (= esn-save-coding-system 1)
                                        ; Do nothing.
     )
   ( (= esn-save-coding-system 2)
     (if esn-w32 
         (esn-dos)
       (esn-unix)))
   ( (= esn-save-coding-system 3)
     (esn-dos))
   ( (= esn-save-coding-system 4)
     (esn-unix))))
(provide 'esn-coding)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-coding.el ends here
