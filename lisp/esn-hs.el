;;; esn-hs.el --- Hide-show code-folding extension for EsN
;; 
;; Filename: esn-hs.el
;; Description: 
;; Author: Matthew L. Fidler
;; Maintainer: 
;; Created: Fri Sep 10 14:56:49 2010 (-0500)
;; Version: 
;; Last-Updated: Mon May  2 12:26:08 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 90
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
;; 29-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep 29 16:56:01 2010 (-0500) #85 (Matthew L. Fidler)
;;    Bug Fix for Esn Hide-show initilization.
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
(declare-function esn-get-current-rec "esn-narrow")
(require 'esn-start)
(require 'hideshow)

(defvar esn-hs-start (concat "\\(?:" (eval-when-compile (esn-reg-records 't nil))
                                  "\\|" esn-mode-indent-keyword-regexp "\\)")
  "* Esn Hide-show start regular expression"
  )

(add-to-list 'hs-special-modes-alist 
             (list 'esn-mode  esn-hs-start "" ";"
                   (lambda (arg) (esn-forward-fold))))
(defun esn-start-folding ()
  "* Starts folding functions for EsN"
  (interactive)
  (hs-minor-mode 1)
  (when (fboundp 'hideshowvis-enable)
    (hideshowvis-enable)
    )
  )
(defun esn-forward-fold (&rest arg)
  "* Used to go to next folded expression in EsN"
  (interactive)
  ;(message "Found %s" arg)
  (cond
   (
    (looking-at (eval-when-compile (esn-reg-records nil 't)))
    ;; Records
    (forward-char (length (match-string 0)))
    (esn-get-current-rec)
    (if esn-fold-record-to-contents-only
        (goto-char esn-get-current-record-stop)
      (goto-char esn-get-current-record-stop2)
      )
    (re-search-backward "\n[ \t]*\\=" nil t)
    )
   ( 
    (looking-at esn-mode-indent-keyword-regexp)
    (let (
          (pt (point))
          (i 1)
          m
          )
      (save-excursion
        (forward-char (length (match-string 0)))
        (re-search-forward (format "\\(%s\\|%s\\)" esn-mode-indent-keyword-regexp esn-mode-deindent-keyword-regexp) nil t)
        (setq m (match-string 0))
        (while (or (if (not (string-match esn-mode-indent-keyword-regexp m))
                       (progn
                         (setq i (- i 1))
                         nil
                         )
                     (setq i (+ i 1))
                     't
                     )
                   (> i 0)
                   )
          (if (not (re-search-forward (format "\\(%s\\|%s\\)" esn-mode-indent-keyword-regexp esn-mode-deindent-keyword-regexp) nil t))
              (setq i -2))
          (setq m (match-string 0))
          )
        (if (>= i 0)
            (setq pt (point))
          (error "Unbalanced expression")
          )
        )
      (goto-char pt)
      )
    )
   )
  )
(provide 'esn-hs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-hs.el ends here
