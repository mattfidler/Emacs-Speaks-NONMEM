;;; esn-cui.el --- EsN's Completion UI interface
;; 
;; Filename: esn-cui.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Wed Sep  8 13:18:38 2010 (-0500)
;; Version: 
;; Last-Updated: Thu Apr 28 06:57:31 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 64
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
;; 06-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Oct  6 09:48:01 2010 (-0500) #35 (Matthew L. Fidler)
;;    Made revision to have completion-ui require a function completion-ui-register-source.
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


(declare-function esn-message "esn-exec")
(eval-when-compile
  (require 'esn-start))

(require 'esn-ac-sources)
(require 'esn-completion)
;; Try to include completion-ui
(require 'completion-ui nil 't)
(when (featurep 'completion-ui)
  (when (fboundp 'completion-ui-register-source)
    (completion-ui-register-source
     (lambda(prefix)
       esn-current-completions)
     :prefix-function 'esn-complete-prefix
     :name 'esn)))

(when (not (fboundp 'complete-esn))
  (defun complete-esn ()
    "* Dummy Completion esn function for autoloads"
    (interactive)
    (esn-message "Could not find the required completion-ui package")))

;;;###autoload
(defun esn-cui-start ()
  "* Start completion UI for EsN."
  (interactive)
  (when (and (= esn-completion-type 3) (fboundp 'auto-completion-mode))
    (esn-completion-off)
    (set (make-local-variable 'auto-completion-source) 'esn)
    (auto-completion-mode 1)))

(defun esn-cui-completed (prefix string &rest ignored)
  "* What happens after Completion-UI sucessfully completes?"
  (esn-after-completion string))

(provide 'esn-cui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-cui.el ends here
