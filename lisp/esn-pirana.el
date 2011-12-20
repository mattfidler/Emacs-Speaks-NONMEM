;;; esn-pirana.el --- EsN's Pirana functions
;; 
;; Filename: esn-pirana.el
;; Description: Pirana Functions
;; Author: Matthew L. Fidler
;; Maintainer:  Matthew L. Fidler
;; Created: Wed Feb 10 23:40:08 2010 (-0600)
;; Version: 23.x
;; Last-Updated: Mon May  2 12:59:22 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 32
;; URL: http://esnm.sourceforge.net
;; Keywords:  Emacs Speaks NONMEM, Pirana
;; Compatibility: Emacs Speaks NONMEM
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
;; 13-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Nov 13 13:10:31 2010 (-0600) #29 (Matthew L. Fidler)
;;    Move `esn-use-pirana' to `esn-which.el'
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 14:30:08 2010 (-0500) #27 (Matthew L. Fidler)
;;    Cache use-pirana result.
;; 08-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep  8 11:02:25 2010 (-0500) #20 (Matthew L. Fidler)
;;    Bugfix for Pirana -- stops loading of control stream
;; 17-Jun-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Jun 17 16:56:04 2010 (-0500) #9 (Matthew L. Fidler)
;;    Save if the buffer is a Pirana buffer.
;; 22-Apr-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Apr 22 12:05:52 2010 (-0500) #4 (Matthew L. Fidler)
;;    Added Esn Error facility
;; 10-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Feb 10 23:40:47 2010 (-0600) #2 (Matthew L. Fidler)
;;    Initial header 
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


(declare-function esn-error "esn-exec")

;; Pirana based functions.  Right now only launch, and put COV on off-diagonal
;; terms, and a different header when EsN detects Pirana is running in the
;; current directory.

(defun esn-pirana-cmd (pirana &rest ARGS)
  "Runs pirana command"
  (setenv "PATH" (with-temp-buffer
                   (insert (getenv "path"))
                   (goto-char (point-min))
                   (while (search-forward "/" nil t)
                     (replace-match "\\" nil t)
                     )
                   (buffer-substring (point-min) (point-max))
                   ))
  (get-buffer-create "*esn-pirana*")
  (save-window-excursion
    (set-buffer "*esn-pirana*")
    (insert "================================================================================\n")
    (insert pirana)
    (insert " ")
    (insert (mapconcat (lambda(x)
                         x
                         ) ARGS " "))
    (insert "\n")
    (insert "\n================================================================================\n")
    )
  (apply 'start-process-shell-command "*esn-pirana*"
         "*esn-pirana*"
         pirana
         ARGS)
  )

(defun esn-pirana-start ()
  "Starts Pirana to run."
  (interactive)
  (let (
        (pirana esn-pirana-exe)
        )
    (if (not (and pirana (not (string= pirana "")) (file-exists-p pirana)))
        (esn-error "Pirana is not specified and cannot be loaded.")
      (esn-pirana-cmd pirana)
      )
    )
  )

(provide 'esn-pirana)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-pirana.el ends here
