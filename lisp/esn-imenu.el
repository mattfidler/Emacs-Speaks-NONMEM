;;; esn-imenu.el --- Imenu interface for EsN
;; 
;; Filename: esn-imenu.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Mon Nov 15 10:24:12 2010 (-0600)
;; Version: 
;; Last-Updated: Wed Apr 27 18:59:37 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 29
;; URL: 
;; Keywords: 
;; Compatibility: 
;; 
;; Features that might be required by this library:
;;
;;   `esn-reg', `esn-start'.
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
(require 'esn-start)
(require 'esn-reg)
(defvar esn-imenu-generic-expression
  (eval-when-compile
    (reverse
     (list
      (list "Records"
            (eval-when-compile (esn-reg-records)) 1)
      (list "Variables"
            "^[ \t]*\\([^ \t\n=]*\\)[ \t]*=[^\n;=].*?\\(?:;.*$\\|$\\)"
            1
            )
      (list "THETA(#)"
            "\\<\\(THETA([0-9]+)\\)" 1)
      (list "ETA(#)"
            "\\<\\(ETA([0-9]+)\\)" 1)
      (list "EPS(#)"
            "\\<\\(EPS([0-9]+)\\)" 1)
      (list "ERR(#)"
            "\\<\\(ERR([0-9]+)\\)" 1))))
  "* Imenu list"
  )

(provide 'esn-imenu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-imenu.el ends here
