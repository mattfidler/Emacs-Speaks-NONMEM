;;; esn-pdx.el --- EsN interactions with PDx pop.
;;
;; Filename: esn-pdx.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Thu Apr 22 12:15:19 2010 (-0500)
;; Version:
;; Last-Updated: Mon May  2 16:20:47 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 107
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
;; 13-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Nov 13 13:07:08 2010 (-0600) #30 (Matthew L. Fidler)
;;    Move esn-use-pdx to esn-which
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 12:21:32 2010 (-0500) #23 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 17-Jun-2010    Matthew L. Fidler
;;    Last-Updated: Thu Jun 17 16:52:48 2010 (-0500) #18 (Matthew L. Fidler)
;;    Save PDX status per buffer.
;; 04-May-2010    Matthew L. Fidler
;;    Last-Updated: Tue May  4 10:44:29 2010 (-0500) #13 (Matthew L. Fidler)
;;    Added support of table.eta and table.par
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

(declare-function esn-runname-noext "esn-properties")
(declare-function esn-update-get-purpose "esn-update")
(declare-function esn-error "esn-exec")

(defun esn-pdx-set-run-number ()
  "* This function sets the  $PROBLEM statement to be RUN# xxx"
  (interactive)
  (let (
        (rn (esn-runname-noext))
        (case-fold-search 't)
        (prp nil)
        )
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (eval-when-compile (esn-reg-record-exp "PRO" nil 't)) nil t)
        (when (looking-at "RUN# [A-Za-z0-9]+")
          (replace-match "")
          )
        (insert "RUN# ")
        (insert rn)
        (insert " -- ")
        (setq prp (esn-update-get-purpose 't))
        (when (string-match "^[ \t]*" prp)
          (setq prp (replace-match "" nil nil prp)))
        (when (string-match "[ \t]*$" prp)
          (setq prp (replace-match "" nil nil prp)))
        (insert prp)
        (when (looking-at ".*")
          (replace-match " "))
        )
      )
    )
  )

(defun esn-PDxPop-cmd (PDxPop &rest ARGS)
  "Runs PDxPop command"
  (setenv "PATH" (with-temp-buffer
                   (insert (getenv "path"))
                   (goto-char (point-min))
                   (while (search-forward "/" nil t)
                     (replace-match "\\" nil t)
                     )
                   (buffer-substring (point-min) (point-max))
                   ))
  (get-buffer-create "*esn-PDxPop*")
  (save-window-excursion
    (set-buffer "*esn-PDxPop*")
    (insert "================================================================================\n")
    (insert PDxPop)
    (insert " ")
    (insert (mapconcat (lambda(x)
                         x
                         ) ARGS " "))
    (insert "\n")
    (insert "\n================================================================================\n")
    )
  (apply 'start-process-shell-command "*esn-PDxPop*"
         "*esn-PDxPop*"
         PDxPop
         ARGS)
  )

(defun esn-PDxPop-start ()
  "Starts PDxPop to run."
  (interactive)
  (let (
        (PDxPop esn-PDxPop-exe)
        )
    (if (not (and PDxPop (file-exists-p PDxPop)))
        (esn-error "PDxPop is not specified and cannot be loaded.")
      (esn-PDxPop-cmd PDxPop))))


(require 'esn-tables)
(defgroup esn-pdx-tables nil
  "PDX tables options"
  :group 'esn-pdx)

(defun esn-pdx-eta-table-name ()
  "Returns pdx eta table name"
  (esn-table-name-extension ".eta"))

(esn-deftable esn-pdx-eta-table
  :table-name esn-pdx-eta-table-name
  :require-etas t
  :add-id t
  :condition (and esn-pdx-generate-tables (esn-use-pdx-p))
  :group 'esn-pdx-tables)

(defun esn-pdx-par-table-name ()
  "Returns pdx par table name"
  (esn-table-name-extension ".par"))

(esn-deftable esn-pdx-par-table
  :table-name esn-pdx-par-table-name
  :require-individual t
  :add-id t
  :condition (and esn-pdx-generate-tables (esn-use-pdx-p))
  :group 'esn-pdx-tables)


(defun esn-pdx-tab-table-name ()
  "Returns pdx tab table name"
  (esn-table-name-extension ".tab"))

(defcustom esn-pdx-tab-sdtab t
  "* Include 'sdtab'-like variables in the conglomarate .tab table."
  :group 'esn-pdx-tables
  :type 'boolean)

(defcustom esn-pdx-tab-categorical t
  "* Include categorical variables in the conglomarate .tab table."
  :group 'esn-pdx-tables
  :type 'boolean)


(defcustom esn-pdx-tab-continuous t
  "* Include continuous variables in the conglomarate .tab table."
  :group 'esn-pdx-tables
  :type 'boolean)

(defcustom esn-pdx-tab-pop t
  "* Include population variables in the conglomarte .tab table"
  :group 'esn-pdx-tables
  :type 'boolean)


(defcustom esn-pdx-tab-ind t
  "* Include individual variables in the conglomarte .tab table"
  :group 'esn-pdx-tables
  :type 'boolean)


(defcustom esn-pdx-tab-err nil
  "* Include error variables in the conglomarte .tab table"
  :group 'esn-pdx-tables
  :type 'boolean)

(defcustom esn-pdx-tab-pred t
  "* Include Prediction variables like PRED, IPRED, PPRED, etc."
  :group 'esn-pdx-tables
  :type 'boolean)


(defcustom esn-pdx-tab-par-res t
  "* Include Residual Variables like RES, IRES, PRES, etc."
  :group 'esn-pdx-tables
  :type 'boolean)


(defcustom esn-pdx-tab-par-iov t
  "* Include IOV parameters like IOV1, BSV1, etc."
  :group 'esn-pdx-tables
  :type 'boolean)

(defcustom esn-pdx-tab-par-eta t
  "* Include ETA parameters like ETA1, ETA2, etc."
  :type 'esn-pdx-tables
  :type 'boolean)



(esn-deftable esn-pdx-tab-table
  :table-name esn-pdx-tab-table-name
  :require-par-iov esn-pdx-tab-par-iov
  :require-error esn-pdx-tab-err
  :require-population esn-pdx-tab-pop
  :require-individual esn-pdx-tab-ind
  :require-pred esn-pdx-tab-pred
  :require-par-res esn-pdx-par-res
  :require-par-eta esn-pdx-tab-par-eta
  :require-other (esn-subset-regexp :categorical esn-pdx-tab-categorical
                                    :continuous esn-pdx-tab-continuous
                                    :sdtab  esn-pdx-tab-sdtab)
  :add-id t
  :condition (and esn-pdx-generate-tables (esn-use-pdx-p))
  :group 'esn-pdx-tables
  )

(provide 'esn-pdx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-pdx.el ends here
