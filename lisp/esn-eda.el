;;; esn-eda.el --- EDA functions for Emacs Speaks NONMEM
;;
;; Filename: esn-eda.el
;; Description: EDA functions for Emacs Speaks NONMEM
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. fidler
;; Created: Thu Mar 11 16:40:42 2010 (-0600)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 94
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM EDA
;; Compatibility: 23.x
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
;; 19-Aug-2010    Matthew L. Fidler  
;;    Tried to standardize record regular expressions.
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

(declare-function esn-rec "esn-properties")

(eval-when-compile
  (require 'esn-start))
(defun esn-eda-get-data-file (&optional data)
  "* Gets the data file from the NONMEM control stream"
  (let (
        (dat (or data (esn-rec "DAT" 't)))
        (case-fold-search 't)
        (ret "NULL")
        )
    (when (string-match (eval-when-compile (esn-reg-data-rec "DAT")) dat)
      (setq ret (concat "\"" (match-string 2 dat) "\""))
      (while (string-match "\\\\" ret)
        (setq ret (replace-match "/" nil nil ret)))
      )
    (symbol-value 'ret)
    )
  )
(defun esn-eda-date-type (&optional input)
  "* Gets DATE Type from NONMEM; Possible types are:  DATE, DAT1, DAT2, and DAT3"
  (let (
        (case-fold-search 't)
        (inp (or input (esn-rec "INP" 't)))
        (ret "NULL")
        )
    (with-temp-buffer
      (insert inp)
      (goto-char (point-min))
      (when (re-search-forward "\\<\\(DAT[E123]\\)[ \t]*=?\\(DROP\\|SKIP\\)\\>" nil t)
        (setq ret (concat "\"" (match-string 1) "\""))
        )
      )
    (symbol-value 'ret)
    )
  )
(defun esn-eda-ignore-char (&optional data)
  "* Gets ignore char for EDA analysis"
  (let (
        (dat (or data (esn-rec "DAT" 't)))
        (ret "NULL")
        (case-fold-search 't)
        )
    (with-temp-buffer
      (insert dat)
      (goto-char (point-min))
      (when (re-search-forward "\\<IGNORE[ \t]*=?[ \t]*[\"']?\\([^(]\\)[\"']?" nil t)
        (setq ret (concat "\"" (match-string 1) "\""))
        )
      )
    (symbol-value 'ret)
    )
  )
(defun esn-eda-ignore (&optional accept data)
  "* Gets ignore=() or accept=() from $DATA record"
  (let (
        (case-fold-search 't)
        (ignore (format "\\<%s[ \t]*=?[ \t]*\\(([^)]*)\\)"
                        (if accept
                            "ACCEPT"
                          "IGNORE")))
        (dat (or data (esn-rec "DAT" 't)))
        (ret "NULL")
        )
    (with-temp-buffer
      (insert dat)
      (goto-char (point-min))
      (when (re-search-forward ignore nil t)
        (setq ret (concat "\"" (match-string 1) "\""))
        )
      )
    (symbol-value 'ret)
    )
  )
(defun esn-eda-input (&optional input)
  "* Gets INPUT code for EDA analysis"
  (let (
        (inp (or input (esn-rec "INP" 't)))
        (case-fold-search 't)
        (ret "NULL")
        )
    (while (string-match ";.*" inp)
      (replace-match ""))
    (with-temp-buffer
      (insert inp)
      (goto-char (point-min))
      (when (re-search-forward (eval-when-compile 
                                 (esn-reg-record-exp "INP")) nil t)
        (replace-match "")
        )
      (goto-char (point-min))
      (while (re-search-forward "=[ \t]*\\(DROP\\|SKIP\\)" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\\(DROP\\|SKIP\\)[ \t]*=" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "[\n\t]" nil t)
        (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t)
        (replace-match " "))
      (goto-char (point-min))
      (when (looking-at "^ +")
        (replace-match ""))
      (goto-char (point-max))
      (skip-chars-backward " ")
      (when (looking-at " +$")
        (replace-match "")
        )
      (goto-char (point-min))
      (while (re-search-forward " " nil t)
        (replace-match "\",\""))
      (goto-char (point-min))
      (insert "c(\"")
      (goto-char (point-max))
      (insert "\")")
      (setq ret (buffer-substring (point-min) (point-max)))
      )
    (symbol-value 'ret)
    )
  )
(provide 'esn-eda)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-eda.el ends here
