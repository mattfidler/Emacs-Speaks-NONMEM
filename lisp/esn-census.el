;;; esn-census.el --- Census Emacs Integration
;; 
;; Filename: esn-census.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Thu Apr 22 12:14:22 2010 (-0500)
;; Version: 
;; Last-Updated: Thu Apr 28 00:56:49 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 16
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
;; 30-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Mon Aug 30 10:22:35 2010 (-0500) #5 (Matthew L. Fidler)
;;    Move census variables
;; 17-Jun-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Jun 17 16:58:16 2010 (-0500) #2 (Matthew L. Fidler)
;;    Added save for census status.
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

;; Census based functions.  Right now only launch.
(defun esn-census-cmd (census &rest ARGS)
  "Runs census command"
  (setenv "PATH" (with-temp-buffer
                   (insert (getenv "path"))
                   (goto-char (point-min))
                   (while (search-forward "/" nil t)
                     (replace-match "\\" nil t)
                     )
                   (buffer-substring (point-min) (point-max))
                   ))
  (get-buffer-create "*esn-census*")
  (save-window-excursion
    (set-buffer "*esn-census*")
    (insert "================================================================================\n")
    (insert census)
    (insert " ")
    (insert (mapconcat (lambda(x)
                         x
                         ) ARGS " "))
    (insert "\n")
    (insert "\n================================================================================\n")
    )
  (apply 'start-process-shell-command "*esn-census*"
         "*esn-census*"
         census
         ARGS)
  )




(defun esn-census-start (&rest ignore)
  "Starts Census to run."
  (interactive)
  (if esn-force-census
      esn-force-census
    (if esn-force-no-census
        nil
      (let (
            (census esn-census-exe)
            )
        (if (not (and census (file-exists-p census)))
            (esn-error "Census is not specified and cannot be loaded.")
          (esn-census-cmd census)
          )
        )
      )))

(provide 'esn-census)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-census.el ends here
