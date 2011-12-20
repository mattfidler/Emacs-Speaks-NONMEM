;;; esn-comint.el --- Provide intervace to comint-complete for EsN
;; 
;; Filename: esn-comint.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Tue Sep  7 09:01:06 2010 (-0500)
;; Version: 
;; Last-Updated: Thu Apr 28 06:51:38 2011 (-0500)
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
;; 07-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep  7 09:46:57 2010 (-0500) #9 (Matthew L. Fidler)
;;    Initial version of generalized completion.  Based on company.
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

(declare-function esn-complete-candidates "esn-company")

(require 'esn-start)
(require 'comint)
(require 'esn-ac-sources)
(require 'esn-completion)
(defun esn-comint-complete ()
  (interactive)
  (let (
        (prefix (esn-complete-prefix))
        lst)
    (setq lst (esn-complete-candidates prefix))
    (when (eq 'sole 
              (comint-dynamic-simple-complete (esn-complete-prefix) lst))
      (when (re-search-backward "[ \t]\\=" nil t)
        (replace-match ""))
      (esn-after-completion (nth 0 (all-completions prefix lst))))))

(provide 'esn-comint)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-comint.el ends here
