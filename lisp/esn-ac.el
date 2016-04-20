;;; esn-ac.el --- Provide Interface to Auto-Complete Mode
;; 
;; Filename: esn-ac.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Aug 20 12:44:53 2010 (-0500)
;; Version: 
;; Last-Updated: Thu Apr 28 00:17:07 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 329
;; URL: 
;; Keywords:
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;  Provides auto-complete mode sources for ESN
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 11:26:11 2010 (-0500) #251 (Matthew L. Fidler)
;;    Options bug-fix
;; 02-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Nov  2 10:06:47 2010 (-0500) #228 (Matthew L. Fidler)
;;    Changed auto-completion items to handle errors "gracefully"
;; 17-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 17 11:43:08 2010 (-0500) #209 (Matthew L. Fidler)
;;    Added Yasnippet to expansion
;; 24-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Aug 24 12:04:53 2010 (-0500) #174 (Matthew L. Fidler)
;;    Change regular expression for records to have to include $.
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
(declare-function esn-completion-off "esn-completion")
(eval-when-compile
  (require 'esn-start))

(require 'esn-ac-sources)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add completion to auto-completion lists.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun esn-ac-start ()
  "Starts auto-completion through auto-complete-mode"
  (interactive)
  (when (fboundp 'auto-complete-mode)
    (auto-complete-mode 1)
    (esn-completion-off)
    (when (boundp 'ac-modes)
      (make-local-variable 'ac-modes)
      (add-to-list 'ac-modes 'esn-mode))
    ;; Add trigger commands
    (add-to-list 'ac-trigger-commands 'esn-upcase-char-self-insert)
    (add-to-list 'ac-trigger-commands 'esn-magic-$)
    (setq ac-sources (append esn-ac-sources ac-sources))
    (auto-complete-mode 1)))


(defun esn-ac ()
  "Call auto-completion."
  (interactive)
  (auto-complete esn-ac-sources))
(provide 'esn-ac)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-ac.el ends here
