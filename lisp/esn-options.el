;;; esn-options.el --- Many Options for Emacs Speaks NONMEM
;;
;; Filename: esn-options.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Wed Jan 27 11:54:10 2010 (-0600)
;; Version: 0.1
;; Last-Updated: Mon May  2 10:18:30 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 776
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Options file.  Imports header options as well.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 09-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Dec  9 00:05:20 2010 (-0600) #755 (Matthew L. Fidler)
;;    Looking for G77 has been paired down levering emacs portable and looking through less drives.
;; 04-Nov-2010
;;    Last-Updated: Thu Nov  4 16:19:18 2010 (-0500) #727 (us041375)
;;    Added `esn-help-wrap-to'
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 09:29:20 2010 (-0500) #724 (Matthew L. Fidler)
;;    Added `esn-highlight-known-vars'
;; 01-Oct-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Oct  1 16:36:19 2010 (-0500) #711 (Matthew L. Fidler)
;;    Added Xmind option to move a control stream to a new topic.
;; 10-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Fri Sep 10 12:57:36 2010 (-0500) #439 (Matthew L. Fidler)
;;    Made font-lock more customizable.
;; 09-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Sep  9 12:12:56 2010 (-0500) #419 (Matthew L. Fidler)
;;    Change files to type 'file.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 11:58:32 2010 (-0600) #5 (Matthew L. Fidler)
;;    Made esn-mode a subgroup of applications.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 11:55:18 2010 (-0600) #2 (Matthew L. Fidler)
;;    Took out backward compatible defcustom.
;; 27-Jan-2010    Matthew L. Fidler
;;    Last-Updated: Wed Jan 27 11:55:10 2010 (-0600) #1 (Matthew L. Fidler)
;;    Added header.
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


(require 'esn-options-header-universal)
;;Take out header update...
(with-temp-buffer
  (insert
   (eval-when-compile
     (let ((ops '("esn-options-header.el"
                  "esn-options-header-small.el"
		  
                  "esn-options-header-pdx.el"
                  "esn-options-header-pdx-small.el"
		  
                  "esn-options-header-plt.el"
                  "esn-options-header-plt-small.el"
		  
                  "esn-options-header-pirana.el"
                  "esn-options-header-pirana-small.el"
		  
                  "esn-options-header-census.el"
                  "esn-options-header-census-small.el"))
           (ret ""))
       (mapc (lambda(x)
               (when (and x (file-exists-p (concat esn-path "lisp/" x)))
                 (let ((tmp "")
                       (tmp2 ""))
                   (if (not (string= "esn-options-header.el" x))
                       (progn
                         (setq tmp x)
                         (when (string-match ".*header\\(.*\\)\\.el$" x)
                           (setq tmp (match-string 1 x)))
                         (when (string= "-small" tmp)
                           (setq tmp "-def-small"))
                         (setq tmp2 tmp)
                         (when (string-match "-small" tmp2)
                           (setq tmp2 (replace-match "" nil nil tmp2)))
                         (with-temp-buffer
                           (insert-file-contents (concat esn-path "lisp/" x))
                           (goto-char (point-min))
                           (while (re-search-forward "(defcustom[ \t\n]*esn-" nil t)
                             (replace-match (concat "(defcustom esn" tmp "-") 't 't))
                           (goto-char (point-min))
                           (while (re-search-forward ":group[^\n()]*" nil t)
                             (replace-match (format ":group 'esn-header%s-options" tmp)))
                           (goto-char (point-min))
                           (insert (format 
                                    "(defgroup esn-header%s-options nil \"%s Header options\" :group 'esn-header%s)" 
                                    tmp
                                    (cond
                                     ( (string= "-plt" tmp)
                                       "PLT Tools Normal")
                                     ( (string= "-plt-small" tmp)
                                       "PLT Tools Small")
                                     ( (string= "-pdx" tmp)
                                       "PDx Pop Normal")
                                     ( (string= "-pdx-small" tmp)
                                       "PDx Pop Small")
                                     ( (string= "-census" tmp)
                                       "Census Normal")
                                     ( (string= "-census-small" tmp)
                                       "Census Small")
                                     ( (string= "-pirana" tmp)
                                       "Pirana Normal")
                                     ( (string= "-pirana-small" tmp)
                                       "Pirana Small")
                                     ( (string= "-def-small" tmp)
                                       "Default Small")
                                     ( 't
                                       "Default Normal XXXXX"))
                                    tmp2))
                           (setq ret (concat ret "\n" (buffer-substring (point-min) (point-max))))))
                     ;; Header Options (default)
                     (with-temp-buffer
                       (insert-file-contents (concat esn-path "lisp/" x))
                       (goto-char (point-min))
                       (while (re-search-forward "(defcustom[ \t\n]*esn-" nil t)
                         (replace-match "(defcustom esn-def-" 't 't))
                       (setq ret (concat ret "\n" (buffer-substring (point-min) (point-max))))
                       (delete-region (point-min) (point-max))
                       (insert-file-contents (concat esn-path  "lisp/" x))
                       (goto-char (point-min))
                       (insert "(defgroup esn-header-def-options nil \"Default Header options\" :group 'esn-header-def)\n")
                       (while (re-search-forward "(defcustom[ \t\n]*\\(esn-[^ \t\n]*\\)" nil t)
                         (setq tmp (match-string 1))
                         (re-search-backward "(")
                         (delete-region (point) (progn (forward-list 1) (point)))
                         (insert "(defun ")
                         (insert tmp)
                         (insert " ()\n")
                         (insert "\"Get Property Heading property for ")
                         (insert tmp)
                         (insert "\"\n")
                         (insert "(if (esn-use-pdx-p) (if esn-header-small ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-pdx-small-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert " ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-pdx-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert ") (if (esn-use-plt-p) (if esn-header-small ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-plt-small-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert " ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-plt-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert ") (if (esn-use-plt-p) (if esn-header-small ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-pirana-small-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert " ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-pirana-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert ") (if (esn-use-census-p) (if esn-header-small ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-census-small-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert " ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-census-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert ") (if esn-header-small ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-def-small-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert " ")
                         (insert (let ((tmp2 tmp))
                                   (when (string-match "esn-" tmp2)
                                     (setq tmp2 (replace-match "esn-def-" 't 't tmp2)))
                                   (symbol-value 'tmp2)))
                         (insert "))))))\n"))
                       (setq ret (concat ret "\n" (buffer-substring (point-min) (point-max)))))))))
             ops)
       (while (string-match "(provide.*?)" ret)
         (setq ret (replace-match "" nil nil ret)))
       (symbol-value 'ret))))
  (eval-buffer))

(defgroup esn-mode nil
  "* Emacs Speaks NONMEM"
  :group 'languages
  )

(defgroup esn-exec nil
  "* EsN executable options"
  :group 'esn-mode
  )

(defgroup esn-80 nil
  "* EsN display of 80+ characters"
  :group 'esn-display
  )

(defcustom esn-exec-g77-base (let (
                                   (ret nil)
                                   )
                               (when esn-w32
                                 (mapc (lambda (x)
                                         (when (and (not ret) x (file-exists-p (concat x "g77/bin")))
                                           (setq ret (concat x "g77"))
                                           )
                                         (when (and (not ret) x (file-exists-p (concat x "G77Portable/App/bin")))
                                           (setq (not ret) (concat x "G77Portable/App"))
                                           )
                                         (when (and (not ret) x (file-exists-p (concat x "PortableApps/G77Portable/App/bin")))
                                           (setq ret (concat x "G77Portable/App"))
                                           )
                                         (when (and (not ret) x (file-exists-p (concat x "PortableApps/g77/bin")))
                                           (setq ret (concat x "PortableApps/g77"))
                                           )
                                         )
                                       (if (boundp 'usb-drive)
                                           (list usb-drive)
                                         (list
                                          "c:/"
                                          "d:/"
                                          "e:/"
                                          "f:/"
                                          )))
                                 (when ret
                                   (while (string-match "/" ret)
                                     (setq ret (replace-match "\\" nil t ret))
                                     )
                                   )
                                 )
                               (symbol-value 'ret)
                               )
  "Base location for G77"
  :type 'file
  :group 'esn-exec
  )

(defcustom esn-exec-plt-check nil
  "Should EsN check for PLT tools before offering support?"
  :type 'boolean
  :group 'esn-plt
  )
(defcustom esn-exec-plt (if esn-w32
                            (progn
                              (if (file-exists-p
                                   "c:\\Program Files\\PLTTools\\PLTTools.exe")
                                  "c:\\Program Files\\PLTTools\\PLTTools.exe"
                                nil
                                )
                              )
                          )
  "The string that defines the location of PLT Tools"
  :type 'file
  :group 'esn-exec
  :group 'esn-plt
  )

(defcustom esn-exec-nmqual (if esn-w32
                               (progn
                                 (if (file-exists-p "c:\\nmvi\\test\\nmvi.pl")
                                     "c:\\nmvi\\test\\nmvi.pl"
                                   (if (file-exists-p "c:\\nmv\\test\\nmvi.pl")
                                       "c:\\nmv\\test\\nmv.pl"
                                     ""
                                     )
                                   )
                                 )
                             (if (file-exists-p "/opt/nmvi/test/nmvi.pl")
                                 "/opt/nmvi/test/nmvi.pl"
                               (if (file-exists-p "/opt/nmv/test/nmvi.pl")
                                   "/opt/nmv/test/nmv.pl"
                                 ""
                                 )
                               )
                             )
  "The string that defines the location of NMQUAL"
  :type 'file
  :group 'esn-exec
  )

(defcustom esn-exec-nm (if esn-w32
                           (progn
                             (if (file-exists-p "c:\\nmvi\\util\\nmfe6.bat")
                                 "c:\\nmvi\\util\\nmfe6.bat"
                               (if (file-exists-p "c:\\nmvii\\util\\nmfe7.bat")
                                   "c:\\nmvii\\util\\nmfe7.bat"
                                 ""
                                 )
                               )
                             )
                         (if (file-exists-p "/opt/nmvi/util/nmfe6")
                             "/opt/nmvi/util/nmfe6"
                           (if (file-exists-p "/opt/nmvii/util/nmfe7")
                               "/opt/nmvii/util/nmfe7"
                             ""
                             )
                           )
                         )
  "The string that defines the location of the NONMEM executable batch file."
  :type 'file
  :group 'esn-exec
  )

(defgroup esn-general-options nil
  "* EsN general Options"
  :group 'esn-mode)


(defcustom esn-save-coding-system 1
  "* Defines if the coding-system is changed, and how it is changed upon save."
  :type '(choice
          (const
           :tag "Keep the file-coding method that the file originally had."
           1)
          (const 
           :tag "Determine the file-coding method from the operating system used."
           2)
          (const
           :tag "Use Windows/Dos encoding"
           3)
          (const
           :tag "Use UNIX/Linux/Max encoding"
           4)
          )
  :group 'esn-general-options
  :group 'esn-changes-on-save
  )
(defgroup esn-record-options nil
  "* Record options"
  :group 'esn-mode)
(defgroup esn-$input-options nil
  "* $INPUT record options"
  :group 'esn-record-options)
(defgroup esn-$subroutines-options nil
  "* $SUBROUTINES record options"
  :group 'esn-record-options)

(defgroup esn-$theta-options nil
  "* $THETA record options"
  :group 'esn-record-options)

(defgroup esn-$omega-options nil
  "* $OMEGA record options"
  :group 'esn-record-options)

(defgroup esn-$sigma-options nil
  "* $SIGMA record options"
  :group 'esn-record-options)

(defgroup esn-$table-options nil
  "* $TABLE record options"
  :group 'esn-record-options
  )
(defgroup esn-$estimate-options nil
  "* $ESTIMATE record options"
  :group 'esn-record-options
  )
(defgroup esn-$nonparametric-options nil
  "* $NONPARAMETRIC record options"
  :group 'esn-record-options
  )

(defgroup esn-$simulation-options nil
  "* $SIMULATION record options"
  :group 'esn-record-options
  )
(defgroup esn-$pred-options nil
  "* $PRED record options"
  :group 'esn-record-options
  )
(defgroup esn-$pk-options nil
  "* $PK record options"
  :group 'esn-record-options
  )

(defgroup esn-automation nil
  "* EsN automation"
  :group 'esn-mode
  )
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changes on Save Options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup esn-changes-on-save nil
  "* EsN Changes on Save"
  :group 'esn-automation)

(defcustom esn-update-list '(
                             "THE"
                             "SUB"
                             "OME"
                             "SIG"
                             "INP"
                             "TAB"
                             )
  "* A list of records to search and indent and align (possibly changing values) before saving.  Just put the first three leters of the record."
  :type '(repeat
          (string :tag "The 3 char $REC")
          )
  :group 'esn-changes-on-save
  :group 'esn-record-options
  )

(defcustom esn-compress-paths nil
  " * Esn-mode will compress paths upon a save.  This means the shortest representation of a file from the current page will be used.
For example if a file is neat or /home/user/neat then neat will be used because it takes less characters."
  :type 'boolean
  :group 'esn-changes-on-save
  )
(defcustom esn-fix-windows-slashes 't
  "* Emacs completes windows slashes with /, fix name completion by replacing / with \\."
  :type 'boolean
  :group 'esn-changes-on-save
  )

(defcustom esn-drop-aliases nil
  "* Drops aliases if they interfer with the one-header output of tables. (I.E. TSFD=TIME,  table requests time, but outputs TSFD as header name.  Change TSFD=TIME to TIME to fix this.)"
  :type 'boolean
  :group 'esn-changes-on-save
  :group 'esn-$input-options
  )

(defgroup esn-automatically-fill-in-tables nil
  "* EsN Options to automatically fill in the tables"
  :group 'esn-changes-on-save
  )
(defgroup esn-plt-tables nil
  "* EsN options to automatically fill in PLT tables."
  :group 'esn-changes-on-save
  :group 'esn-plt
  )
(defgroup esn-single-table nil
  "* EsN options to automatically fill in a single table."
  :group 'esn-automatically-fill-in-tables
  )

(defcustom esn-update-table-continuous-covariates 't
  "* If not nil, then esn-mode will update the continuous covariates in the table corresponding to the run.
Table defined by `esn-xpose-cotab'
"
  :type 'boolean
  :group 'esn-single-table)


(defcustom esn-update-table-categorical-covariates 't
  "* If not nil, then esn-mode will update the continuous covariates in the table corresponding to the run.
Table defined by `esn-xpose-catab'
"
  :type 'boolean
  :group 'esn-single-table)

(defcustom esn-update-table-extra  nil
  "* If not nil, then esn-mode will update the extra variables in the table corresponding to the run.
Table defined by `esn-xpose-extra'
"
  :type 'boolean
  :group 'esn-single-table)

(defcustom esn-update-table-xptab  nil
  "* If not nil, then esn-mode will update the extra variables in the table corresponding to the run.
Table defined by `esn-xpose-xptab'
"
  :type 'boolean
  :group 'esn-single-table)

(defcustom esn-update-table-mytab  nil
  "* If not nil, then esn-mode will update the extra variables in the table corresponding to the run.
Table  defined by `esn-xpose-mytab'
"
  :type 'boolean
  :group 'esn-single-table)

(defcustom esn-update-table-mutab  nil
  "* If not nil, then esn-mode will update the Multiple Response variables in the table corresponding to the run.
Table defined by `esn-xpose-mutab'
"
  :type 'boolean
  :group 'esn-single-table)

(defcustom esn-update-table-sdtab  't
  "* If not nil, then esn-mode will update the standard variables in the table corresponding to the run.
Table defined by `esn-xpose-sdtab'
"
  :type 'boolean
  :group 'esn-single-table)

(defcustom esn-update-table-cwtab  't
  "* If not nil, then esn-mode will update the conditional weighted residuals in the table corresponding to the run.
"
  :type 'boolean
  :group 'esn-single-table)

(defcustom esn-update-table-patab  't
  "* If not nil, then esn-mode will update the conditional weighted residuals in the table corresponding to the run.
"
  :type 'boolean
  :group 'esn-single-table)


(defgroup esn-update-file-names nil
  "* EsN updating of file-names on save."
  :group 'esn-changes-on-save
  )


                                        ; MSFO updating
(defgroup esn-update-msfo-file nil
  "* EsN updating of MSFO file names on save"
  :group 'esn-update-file-names)

(defcustom esn-update-msfo nil
  "* If not nil, then esn-mode will update the table when (1) MSFO=.msf or (2) MSFO=currentfile.msf where currentfile represents the file that is being edited."
  :type 'boolean
  :group 'esn-mode-msfo-file
  :group 'esn-$estimate-options
  :group 'esn-$nonparametric-options
  )


(defcustom esn-msfo-start "\\<MSFO *= *"
  "The regular expression that defines where the MSFO should start."
  :type 'regexp
  :group 'esn-update-msfo-file
  :group 'esn-$estimate-options
  :group 'esn-$nonparametric-options
  )
(defcustom esn-msfo-est ".msf"
  "The extension for estimation MSFOs."
  :type 'string
  :group 'esn-update-msfo-file
  :group 'esn-$estimate-options
  :group 'esn-naming
  )
(defcustom esn-msfo-non ".nsf"
  "The extension for NONPARAMETRIC MSFOs."
  :type 'string
  :group 'esn-update-msfo-file
  :group 'esn-$nonparametric-options
  :group 'esn-naming
  )
                                        ; Fortran updating
(defgroup esn-update-fortran-file nil
  "* EsN updating of fortran OPEN type files"
  :group 'esn-update-file-names
  )
(defcustom esn-update-fortran nil
  "* If not nil, then esn-mode will update the FORTRAN Open blocks."
  :type 'boolean
  :group 'esn-update-fortran-file
  )

(defcustom esn-fortran-start "\\<OPEN *( *[0-9]+ *, *FILE *= *"
  "The regular expression that defines where the fortran update should take place."
  :type 'regexp
  :group 'esn-update-fortran-file
  )

(defcustom esn-fortran-stop ")"
  "The regular expression that defines where the fortran update should stop."
  :type 'regexp
  :group 'esn-update-fortran-file
  )



                                        ; Tables
(defgroup esn-update-table-file nil
  "* EsN update TABLE file names"
  :group  'esn-update-file-names
  :group 'esn-$table-options
  )
(defcustom esn-update-table nil
  "* If not nil, then esn-mode will update the table file. Without any other options selected, this updates the table whenever (1) FILE=table or (2) FILE=currentfile where currentfile represents the file that is being edited."
  :type 'boolean
  :group 'esn-update-table-file
  :group 'esn-$table-options
  )

(defcustom esn-table-extension ".tbl"
  "The string that defines the table extension whenever Xpose is not used and table names are updated."
  :type 'string
  :group 'esn-update-table-file
  :group 'esn-$table-options
  :group 'esn-naming
  )

(defcustom esn-table-start (format "\\<FILE *[ =\t] *%s" (regexp-quote esn-completing-current-directory))
  "The regular expression that defines where the table update should take place."
  :type 'regexp
  :group 'esn-mode-table-file
  :group 'esn-$table-options
  )

(defcustom esn-update-table-base-name-replaced 't
  "* If not nil, then esn-mode will update any table that has the base file name in it that used to correspond to the old file name."
  :type 'boolean
  :group 'esn-mode-table-file
  :group 'esn-$table-options
  )

(defcustom esn-update-table-force-update-if-only-one-output 't
  "* If not nil, then esn-mode will update any table output to the current file if there is only one table file outputted."
  :type 'boolean
  :group 'esn-mode-table-file
  :group 'esn-$table-options
  )


(defgroup esn-changes-while-editing nil
  "* EsN Changes while editing"
  :group 'esn-automation
  )


(defgroup esn-units nil
  "* Options to try to get units from control stream"
  :group 'esn-mode)

(defcustom esn-xpose-weight-variables
  '(
    "WTKG"
    "WT"
    "KGWT"
    "WTLB"
    "LBWT"
    "WGT"
    "WGHT"
    "WGTK"
    "KWGT"
    "LWGT"
    "WGTL"
    )
  "* Defines the possible INPUT varibales for weight.  This is used to calculate the scale factor when the dose is weight adjusted."
  :type '(repeat
          (string :tag "Weight Variable.")
          )
  :group 'esn-units)

(defcustom esn-xpose-bsa-variables
  '(
    "BSA"
    "BDSA"
    )
  "* Defines the possible INPUT varibales for body surface area.  This is used to calculate the scale factor when the dose is body-surface-area adjusted."
  :type '(repeat
          (string :tag "Weight Variable.")
          )
  :group 'esn-units)

(defcustom esn-variable-labels
  '(
    ("AB" "")
    ("ADDL" "Additional Doses")
    ("AGE" "Age (yr)")
    ("AI" "A coefficient for infusion")
    ("ALPHA" "ALPHA Elimination Phase")
    ("ALT" "Alanine Aminotransferase (U/L)")
    ("AMT" "Amount of Dose")
    ("AOB" "A/B")
    ("AST" "Aspartate Aminotransferase (U/L)")
    ("AUC" "Area Under the Curve")
    ("BB" "")
    ("BETA" "BETA Elimination Phase")
    ("BI" "B Coefficient for infusion")
    ("BIL" "Bilirubin (mg/dL)")
    ("BMI" "Body Mass Index (unitless)")
    ("BSA" "Body Surface Area (m^2)")
    ("BUN" "Urea nitrogen, blood (mg/dL)")
    ("CA" "A coefficient")
    ("CALL" "Force procedure (0: normal; 1: error, 2: PK, 3: PK&Error, 10: event)")
    ("CB" "B coefficient")
    ("CDSE" "Dose as continuous covariate")
    ("CHRT" "Cohort")
    ("CL" "Clearance")
    ("CMAX" "Maximum Concentration")
    ("CMT" "Compartment Number")
    ("CO2" "Bicarbonate and CO2 (mEq/L)")
    ("CONT" "Continuation Data Item (0: last/only record, 1: this record & next same event)")
    ("CP" "Plasma Concentration")
    ("CPK" "Creatine Kinase (U/L)")
    ("CRCL" "Creatinine Clearance (mL/min)")
    ("CST" "Concentration Status (0: Missing, 1: BLQ, 2: ALQ, 3: NR, 4: NS, 5: NA)")
    ("CTDS" "Dose as continuous covariate")
    ("DAT1" "Date (Formatted DD/MM/YY)")
    ("DAT2" "Date (Formatted YY/MM/DD)")
    ("DAT3" "Date (Formatted YY/DD/MM)")
    ("DATE" "Date (Formatted MM/DD/YY)")
    ("DAY" "Study Day")
    ("DNMB" "Id Number")
    ("DOSE" "Dose as categorical covariate")
    ("DSE" "Dose as categorical covariate")
    ("DSTM" "Dosing Time")
    ("DUR" "Duration of Infusion (hr)")
    ("DV" "Dependent Variable")
    ("DV1" "Dependent Variable #1")
    ("DV10" "Dependent Variable #10")
    ("DV2" "Dependent Variable #2")
    ("DV3" "DependentVariable #3")
    ("DV4" "Dependent Variable #4")
    ("DV5" "Dependent Variable #5")
    ("DV6" "Dependent Variable #6")
    ("DV7" "Dependent Variable #7")
    ("DV8" "Dependent Variable #8")
    ("DV9" "Dependent Variable #9")
    ("DVY" "Dependent Variable (YLO)")
    ("ETHN" "Ethnicity")
    ("EVID" "Event ID (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID1" "Event ID #1 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID10" "Event ID #10 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID2" "Event ID #2 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID3" "Event ID #3 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID4" "Event ID #4 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID5" "Event ID #5 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID6" "Event ID #6 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID7" "Event ID #7 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID8" "Event ID #8 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EVID9" "Event ID #9 (0: Obs., 1: dose, 2: other, 3: reset, 4: reset & dose)")
    ("EYE" "Eye")
    ("F1" "Bioavailability 1")
    ("F2" "Bioavailability 2")
    ("F3" "Bioavailability 3")
    ("FAST" "Food (0: Fed, 1: Fasted)")
    ("FED" "Food (0: Fasted, 1: Fed)")
    ("FST" "Food (0: Fed, 1: Fasted)")
    ("GEND" "Gender")
    ("GGT" "Gamma-glutamyl transerfase (U/L)")
    ("GNDF" "Gender (0: Male, 1: Female)")
    ("GNDM" "Gender (0: Female, 1: Male)")
    ("GNDR" "Gender")
    ("HDL" "HDL Cholesterol (mg/dL)")
    ("HGHT" "Height")
    ("HINP" "Hepatic Impairment")
    ("HLA" "Half Life of Alpha elimination phase")
    ("HLB" "Half Life of Beta elimination phase")
    ("HLK" "Half Life")
    ("HT" "Height")
    ("HTCM" "Height (cm)")
    ("HTFT" "Height (ft)")
    ("HTIN" "Height (in)")
    ("HTM" "Height (m)")
    ("ICLR" "Iris Color")
    ("ID" "NONMEM ID")
    ("II" "Interdose Interval")
    ("INV" "Investigator ID")
    ("IPRED" "Individual Predictions")
    ("IRES" "Individual Residuals")
    ("IWRES" "Individual Weighted Residuals")
    ("K" "Elimination Rate")
    ("K12" "Rate of Compartment 1 to 2")
    ("K21" "Rate of Compartment 2 to 1")
    ("KA" "Absorption Rate")
    ("L1" "Subject Label")
    ("L2" "Level two label (same L2=same epsilons)")
    ("LAG" "Lag Time")
    ("LBM" "Lean Body Mass (kg)")
    ("LDH" "Lactate Dehydrogenase (U/L)")
    ("LDL" "LDL Cholesterol (mg/dL)")
    ("LDV" "Log(Dependent Variable)")
    ("LGDV" "Log(Dependent Variable)")
    ("LGMT" "Log(Metabolite)")
    ("LGPR" "Log(Parent)")
    ("LLOQ" "Lower Limit of Quantitation")
    ("LOGM" "Log(Metabolite)")
    ("LOGP" "Log(Parent)")
    ("MDV" "Missing Dependent Variable (0: Present, 1: Missing)")
    ("MET" "Metabolite")
    ("MSSV" "Missing Visit")
    ("MTBL" "Metabolite")
    ("NA" "Sodium (mEq/L)")
    ("NMNL" "Nominal Time (hr)")
    ("NMTM" "Nominal Time (hr)")
    ("NTME" "Nominal Time (hr)")
    ("NUM" "Observation Number")
    ("OBS" "Observation Number")
    ("OCC" "Occassion")
    ("PAR" "Parent")
    ("PAT" "Patient ID")
    ("PCMT" "Prediction Compartment")
    ("PNUM" "Profile Number")
    ("PRED" "Population Predictions")
    ("PRNT" "Parent")
    ("PTNT" "Patient Initials")
    ("Q" "Distributional Clearance")
    ("Q1" "Distributional Clearance 1")
    ("Q2" "Distributional Clearance 2")
    ("Q3" "Distributional Clearance 3")
    ("RACE" "Race")
    ("RATE" "Rate of Dose administration (AMT/TIME)")
    ("RCEN" "Race")
    ("REMP" "Renal Impairment")
    ("RES" "Population Residuals")
    ("RINP" "Renal Impairment")
    ("SBJC" "Subject")
    ("SBJD" "Subject")
    ("SCR" "Serum Creatinine (mg/dL)")
    ("SEX" "Gender")
    ("SEXF" "Gender (0: Male, 1: Female)")
    ("SEXM" "Gender (0: Female, 1: Male)")
    ("SGOT" "Aspartate Aminotransferase (U/L)")
    ("SGPT" "Alanine Aminotransferase (U/L)")
    ("SMOK" "Smoker(0: Non-smoker, 1: Smoker)")
    ("SS" "Steady State (0: Not steady-state, 1: steady-state, 2:steady-state superpositioned)")
    ("STDY" "Study")
    ("TAD" "Time After Dose (hr) ")
    ("TBIL" "Total Bilirubin (mg/dL)")
    ("TIBC" "Iron-binding capacity (g/dL)")
    ("TIME" "Time of Event (hr)")
    ("TMDV" "Time Deviation")
    ("TRT" "Treatment")
    ("TRT1" "Treatment #1")
    ("TRT2" "Treatment #2")
    ("TRT3" "Treatment #3")
    ("TSFD" "Time Since First Dose (hr)")
    ("TSLD" "Time Since Last Dose (hr)")
    ("TT3" "Triiodothyronine (ng/dL)")
    ("TT4" "Thyroxine (ug/dL)")
    ("V" "Volume of Distribution")
    ("V1" "Volume of Compartment 1")
    ("V2" "Volume of Compartment 2")
    ("V3" "Volume of Compartment 3")
    ("VC" "Central Volume")
    ("VP" "Peripheral Volume")
    ("VP1" "Peripheral Volume 1")
    ("VP2" "Peripheral Volume 2")
    ("VSS" "Steady State Volume")
    ("VSTN" "Visit Number")
    ("VSTN" "Visit Number")
    ("WGHT" "Weight (kg)")
    ("WRES" "Weighted Residuals")
    ("WT" "Weight (kg)")
    ("WTKG" "Weight (kg)")
    ("WTLB" "Weight (lb)")
    )
  "* Defines the variable labels for known variables"
  :type '(repeat
          (list
           (string :tag "NONMEM variable")
           (string :tag "Variable Label")
           )
          )
  :group 'esn-completion
  :group 'esn-$input-options
  )
(defcustom esn-mode-dv-vars '("CP" "LGCP" "LNCP" "PAR")
  "* Defines the variables assumed to be DV."
  :type '(repeat (string :tag "DV variable"))
  :group 'esn-completion
  :group 'esn-$input-options
  )

(defcustom esn-mode-date-vars '("DATE" "DAT1" "DAT2")
  "* Defines the variables assumed to be Dates, Don't include the variable itself."
  :type '(repeat (string :tag "DATE variable"))
  :group 'esn-add-information-while-typing
  :group 'esn-$input-options
  )
(defcustom esn-mode-time-vars '("TSFD")
  "* Defines the variables assumed to be TIME, Don't include the variable itself."
  :type '(repeat (string :tag "TIME variable"))
  :group 'esn-completion
  :group 'esn-$input-options
  )                                     ;
(defcustom esn-mode-reserved-vars '("AMT" "RATE" "DUR" "OCC"
                                    "CMT" "PCMT" "CONT" "L1" "L2"
                                    "DUR" "TIME"
                                    "II" "SS" "ADDL"
                                    )
  "* Defines NONMEM defined variables that are not dropped when completing the $INPUT record."
  :type '(repeat (string :tag "Variable"))
  :group 'esn-completion
  :group 'esn-$input-options
  )
(defcustom esn-mode-tab-complete-input-after-data 't
  "* If not nil, After ESN-mode tab-completes the $INPUT record, $DATA is changes position to be before $INPUT, and the cursor position ends up after the $DATA record."
  :type 'boolean
  :group 'esn-completion
  :group 'esn-$input-options
  )
(defcustom esn-mode-tab-complete-input 't
  "* If not nil, ESN-mode TAB-completes the $INPUT record to have the headings in the CSV file."
  :type 'boolean
  :group 'esn-completion
  :group 'esn-$input-options
  )

(defcustom esn-mode-tab-complete-input-size 1400
  "* Describes the maximum size (in bytes) that EsN will open when looking at a input file to get line."
  :type 'boolean
  :group 'esn-completion
  :group 'esn-$input-options
  )

(defgroup esn-add-information-while-typing nil
  "* Information added to comments in the control stream while typing."
  :group 'esn-automation
  )
(defcustom esn-update-input 't
  "* If not nil, then esn-mode will count the number of input items retained."
  :type 'boolean
  :group 'esn-add-information-while-typing
  :group 'esn-$input-options
  )

(defcustom esn-table-pred-display 't
  "* Defines if the number of generated variables are displayed and updated."
  :type 'boolean
  :group 'esn-code
  :group 'esn-add-information-while-typing
  :group 'esn-$table-options
  )
(defcustom esn-table-split-pred nil
  "* Defines whether to split a file to allow more than 20 variables output."
  :type 'boolean
  :group 'esn-code
  :group 'esn-$table-options
  :group 'esn-changes-on-save
  )


(defcustom esn-update-sub 't
  "* If not nil, then esn-mode will add a comment about the ADVAN/TRANS combination used with the subroutines record."
  :type 'boolean
  :group 'esn-add-information-while-typing
  :group 'esn-$subroutines-options
  )
(defcustom esn-update-sub-small 't
  "* If not nil, the esn-mode will add add a small comment about the ADVAN/TRANS combination used with the subroutines record."
  :type 'boolean
  :group 'esn-add-information-while-typing
  :group 'esn-$subroutines-options
  )

(defcustom esn-update-input-add-comment nil
  "* If not nil, Add comments about each variable in the $INPUT field"
  :type 'boolean
  :group 'esn-add-information-while-typing
  :group 'esn-$input-options
  )

(defgroup esn-key-behaviors nil
  "* EsN options for key behaviors"
  :group 'esn-changes-while-editing
  )
(defcustom esn-fix-records 't
  "* If not nil, esn-mode will make sure that records are at the beginning of a line when wrapping OR pressing $"
  :type 'boolean
  :group 'esn-key-behaviors
  :group 'esn-wrapping
  )

(defcustom esn-caps-tool 't
  " * If non-nil (selected) ESN-mode will type in all caps at the appropriate places."
  :type 'boolean
  :group 'esn-key-behaviors
  )


(defcustom esn-sim-add-seed 't
  "* Defines if pressing the ( in the $SIM record produces a seed."
  :type 'boolean
  :group 'esn-key-behaviors
  :group 'esn-$simulation-options
  )

(defcustom esn-use-magic-help-key 't
  "* Defines if Emacs Speaks NONMEM uses the magic help key."
  :type 'boolean
  :group 'esn-key-behaviors
  :group 'esn-help
  )

(defgroup esn-wrapping nil
  "* EsN wrapping of records"
  :group 'esn-changes-while-editing
  )

(defcustom esn-wrapping-of-records 't
  "* If not nil, esn-mode will wrap the records upon pressing a space or return."
  :type 'boolean
  :group 'esn-wrapping
  )
(defcustom esn-records-not-wrapped '(
                                     "BIN"
                                     "DES"
                                     "ERR"
                                     "ERR"
                                     "INF"
                                     "INP"
                                     "MIX"
                                     "MOD"
                                     "OME"
                                     "PK"
                                     "PRE"
                                     "PRO"
                                     "SIG"
                                     "THE"
                                     "THT"
                                     )
  "* A list of records to skip wrapping on (possibly changing values) before saving.  Just put the first three leters of the record.

The $INPUT and $BIND records are wrapped by a different mechanism (to allow interleaving and appropriate spacing)
"
  :type '(repeat
          (string :tag "The 3 char $REC")
          )
  :group 'esn-wrapping
  :group 'esn-record-options
  )

(defgroup esn-version-control nil
  "* EsN Version Control"
  :group 'esn-automation
  )
(defcustom esn-use-version-control nil
  "Defines if version control should be used."
  :type 'boolean
  :group 'esn-header-universal
  :group 'esn-version-control
  )

(defcustom esn-prompt-to-edit-file-with-newer-outputs nil
  "* Prompt to edit a control stream with newer outputs"
  :type 'boolean
  :group 'esn-version-control
  )

(defcustom esn-vc-backend "hg"
  "* Backend for automatic versioning.
hg      = Mercurial
git     = GIT
bzr     = Bazaar
darcs   = Darcs
rcs     = RCS
"
  :type '(choice
          (string :tag "Mercurial" :value "hg")
          (string :tag "GIT" :value "git")
          (string :tag "Bazaar" :value "bzr")
          (string :tag "Darcs" :valuie "darcs")
          (string :tag "RCS" :value "rcs")
          )
  :group 'esn-version-control
  )

(defcustom esn-vc-mode-hook nil
  "Hook run when a buffer is created to prepare a commit."
  :type 'sexp
  :group 'esn-version-control
  )


(defgroup esn-code-generation nil
  "* EsN automatic code generation"
  :group 'esn-automation
  )

(defcustom esn-mode-generate-secondary-parameters nil
  "* If not nil, EsN mode generates secondary parameters on save."
  :type 'boolean
  :group 'esn-code-generation
  )

(defgroup esn-cwres nil
  "*EsN CWRES code generation"
  :group 'esn-code-generation
  )

(defcustom esn-cwres-psn nil
  "* Defines if PsN naming convention is assumed.  If it is assumed, output the estimate file in ../../.  This will allow Xpose to calculate conditional weighted residuals."
  :type 'boolean
  :group 'esn-cwres
  :group 'esn-psn
  )



(defcustom esn-assumed-version "6"
  "* The assumed version of NONMEM for ESN."
  :type 'string
  :group 'esn-cwres
  :group 'esn-code-generation
  :group 'esn-general-options
  )

(defcustom esn-cwres-estimate-extension ".est"
  "* Defines the default extension for the ETA, THETA, OMEGA, and SIGMA output file used to calculate CWRES"
  :type 'string
  :group 'esn-cwres
  :group 'esn-$infn-options
  )
(defcustom esn-cwres-infn5-pred-template-1
  "\"FIRST
\"      COMMON /ROCM6/ THETAF(40),OMEGAF(30,30),SIGMAF(30,30)
\"      COMMON /ROCM7/ SETH(40),SEOM(30,30),SESIG(30,30)
\"      COMMON /ROCM8/ OBJECT
\"      DOUBLE PRECISION THETAF, OMEGAF, SIGMAF
\"      DOUBLE PRECISION OBJECT
\"      REAL SETH,SEOM,SESIG
\"      INTEGER J,I
\"      INTEGER MODE
\"      INTEGER NTH,NETA,NEPS
\"      DATA NTH,NETA,NEPS/4,3,1/
"
  "* Template for INFN block using CWRES assuming NONMEM 5, PRED procedure, Part 1."
  :type 'string
  :group 'esn-cwres
  :group 'esn-$infn-options
  )
(defcustom esn-cwres-infn5-pred-template-2
  "\"      IF (ICALL.EQ.0) THEN
\"C     open files here, if necessary
\"         OPEN(50,FILE='filename.est')
\"      ENDIF
\"      IF (ICALL.EQ.3) THEN
\"         MODE=0
\"         CALL PASS(MODE)
\"         MODE=1
\"      WRITE(50,*) 'ETAS'
\" 20      CALL PASS(MODE)
\"         IF (MODE.EQ.0) GO TO 30
\"         IF (NEWIND.NE.2) THEN
\"            CALL GETETA(ETA)
\"            WRITE (50,97) (ETA(I),I=1,NETA)
\"         ENDIF
\"         GO TO 20
\" 30      CONTINUE
\"         WRITE (50,*) 'THETAS'
\"         WRITE (50,99) (THETAF(J),J=1,NTH)
\"         WRITE (50,*) 'OMEGAS'
\"         DO 7000 I=1,NETA
\" 7000       WRITE (50,99) (OMEGAF(I,J),J=1,NETA)
\"         WRITE (50,*) 'SIGMAS'
\"         DO 7999 I=1,NEPS
\" 7999       WRITE (50,99) (SIGMAF(I,J),J=1,NEPS)
\"      ENDIF
\" 99   FORMAT (20E15.7)
\" 98   FORMAT (2I8)
\" 97   FORMAT (10E15.7)
"
  "* Template for INFN block using CWRES assuming NONMEM 5, PRED procedure, Part 2."
  :type 'string
  :group 'esn-cwres
  :group 'esn-$infn-options
  )


(defcustom esn-cwres-infn5-advan-template
  "      SUBROUTINE INFN(ICALL,THETA,DATREC,INDXS,NEWIND)
      DIMENSION THETA(*),DATREC(*),INDXS(*)
      DOUBLE PRECISION THETA
      COMMON /ROCM6/ THETAF(40),OMEGAF(30,30),SIGMAF(30,30)
      COMMON /ROCM7/ SETH(40),SEOM(30,30),SESIG(30,30)
      COMMON /ROCM8/ OBJECT
      COMMON /ROCM9/ IERE,IERC
      DOUBLE PRECISION THETAF, OMEGAF, SIGMAF
      DOUBLE PRECISION OBJECT
      REAL SETH,SEOM,SESIG
      DOUBLE PRECISION ETA(10)
      INTEGER J,I
      INTEGER IERE,IERC
      INTEGER MODE
      INTEGER NTH,NETA,NEPS
      DATA NTH,NETA,NEPS/4,3,1/
      IF (ICALL.EQ.0) THEN
C     open files here, if necessary
         OPEN(50,FILE='filename.est')
      ENDIF
      IF (ICALL.EQ.3) THEN
         MODE=0
         CALL PASS(MODE)
         MODE=1
         WRITE(50,*) 'ETAS'
20       CALL PASS(MODE)
         IF (MODE.EQ.0) GO TO 30
         IF (NEWIND.NE.2) THEN
            CALL GETETA(ETA)
            WRITE (50,97) (ETA(I),I=1,NETA)
         ENDIF
         GO TO 20
30       CONTINUE
         WRITE (50,*) 'THETAS'
         WRITE (50,99) (THETAF(J),J=1,NTH)
         WRITE(50,*) 'OMEGAS'
         DO 7000 I=1,NETA
7000       WRITE (50,99) (OMEGAF(I,J),J=1,NETA)
         WRITE(50,*) 'SIGMAS'
         DO 7999 I=1,NEPS
7999       WRITE (50,99) (SIGMAF(I,J),J=1,NEPS)
      ENDIF
99    FORMAT (20E15.7)
98    FORMAT (2I8)
97    FORMAT (10E15.7)
      RETURN
      END
"
  "* Template for INFN block using CWRES assuming NONMEM 5 ADVAN procedure."
  :type 'string
  :group 'esn-cwres
  :group 'esn-$infn-options
  )
(defcustom esn-cwres-infn-template "
$INFN
  IF(ICALL.EQ.3)THEN
    OPEN(50,FILE='filename.est')
    WRITE(50,*) 'ETAS'
    DO WHILE(DATA)
      IF(NEWIND.LE.1) WRITE(50,*) ETA
    ENDDO
    WRITE(50,*) 'THETAS'
    WRITE(50,*) THETA
    WRITE(50,*) 'OMEGAS'
    WRITE(50,*) OMEGA(BLOCK)
    WRITE(50,*) 'SIGMAS'
    WRITE(50,*) SIGMA(BLOCK)
  ENDIF
"
  "* Template for INFN block using CWRES."
  :type 'string
  :group 'esn-cwres
  :group 'esn-$infn-options
  )
(defcustom esn-hide-cwres-generated-code 't
  "* Hide cwres generated code."
  :type 'boolean
  :group 'esn-cwres
  :group 'esn-hidden
  )
(defcustom esn-automatically-generate-cwres nil
  "* Automatically generate CWRES when appropriate."
  :type 'boolean
  :group 'esn-cwres
  )

(defcustom esn-start-hideshow-code-folding nil
  "* Start hideshow code folding on startup."
  :type 'boolean
  :group 'esn-hidden
  )

(defcustom esn-fold-record-to-contents-only nil
  "* Fold records to contents ONLY."
  :type 'boolean
  :group 'esn-hidden
  )

(defcustom esn-cwres-foce 't
  "* If CWRES are generated for FOCE."
  :type 'boolean
  :group 'esn-cwres
  )
(defcustom esn-cwres-focei nil
  "* If CWRES are generated for FOCEI."
  :type 'boolean
  :group 'esn-cwres
  )
(defcustom esn-cwres-fo-posthoc nil
  "* If CWRES are generated for FO."
  :type 'boolean
  :group 'esn-cwres
  )
(defcustom esn-cwres-foi-posthoc nil
  "* If CWRES are generated for FOI."
  :type 'boolean
  :group 'esn-cwres
  )
(defcustom esn-cwres-hybrid nil
  "* If CWRES are generated for Hybrid."
  :type 'boolean
  :group 'esn-cwres
  )
(defcustom esn-cwres-lap nil
  "* Automatically generate CWRES for Laplacian models."
  :type 'boolean
  :group 'esn-cwres
  )

(defcustom esn-cwres-remove 't
  "* Automatically remove CWRES for unsupported estimation methods."
  :type 'boolean
  :group 'esn-cwres
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Header options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup esn-header nil
  "* EsN header options"
  :group 'esn-mode
  )

(defcustom esn-build-new-buffer 't
  "* If not nil, then esn-mode will insert a header when new buffers are generated"
  :type 'boolean
  :group 'esn-header
  )

(defcustom esn-update-problem-file-name 't
  "* Updates PROBLEM statement to reflect file name."
  :type 'boolean
  :group 'esn-header
  )
(defgroup esn-header-universal nil
  "* EsN header universal options"
  :group 'esn-header
  )

(defgroup esn-pdx nil
  "* Esn Pdx options"
  :group 'esn-mode
  )
(defcustom esn-PDxPop-exe
  (if esn-w32
      (progn
        (if (file-exists-p
             "c:\\Program Files\\PDxPop\\PDxPop.exe")
            "c:\\Program Files\\PDxPop\\PDxPop.exe"
          (if (file-exists-p "c:\\PDxPop3\\PDxPop.exe")
              "c:\\PDxPop3\\PDxPop.exe"
            (if (file-exists-p "c:\\PDxPop4\\PDxPop.exe")
                "c:\\PDxPop4\\PDxPop.exe"
              ""
              )
            )
          )
        )
    )
  "The string that defines the location of PDx Pop"
  :type 'file
  :group 'esn-pdx
  :group 'esn-exec
  )


(defcustom esn-mode-use-pdx nil
  "* Use PDX pop options for building control streams"
  :type 'boolean
  :group 'esn-pdx
  )

(defcustom esn-pdx-choose-file-name nil
  "* Use Emacs Speaks NONMEM to choose #.ctl with PDx pop control streams."
  :type 'boolean
  :group 'esn-pdx
  :group 'esn-naming
  )

(defcustom esn-pdx-choose-file-name-padding nil
  "* Use Emacs Speaks NONMEM to choose ###.ctl with PDx pop control streams."
  :type 'boolean
  :group 'esn-pdx
  :group 'esn-naming
  )

(defcustom esn-mode-use-omega-sigma-var-type-labels 't
  "* Use PDX pop's variability labels:
;[A] represents additive variability
;[P] represents exponential or proportional variability
;[F] represents off-diagonal terms"
  :type 'boolean
  :group 'esn-pdx
  :group 'esn-$omega-options
  :group 'esn-$sigma-options
  )

(defcustom esn-mode-auto-generate-brackets-for-theta 't
  "* Automatically generate brackets for labels in $THETA"
  :type 'boolean
  :group 'esn-pdx
  :group 'esn-$theta-options
  :group 'esn-key-behaviors
  )
(defcustom esn-pdx-gen-xpose 't
  "* Generate Xpose table for PDx-Pop"
  :type 'boolean
  :group 'esn-pdx
  :group 'esn-$table-options
  )
(defcustom esn-pdx-gen-one-table 't
  "* Generate table.tab for PDx-Pop"
  :type 'boolean
  :group 'esn-pdx
  :group 'esn-$table-options
  )
(defcustom esn-pdx-gen-eta-table 't
  "* Generate table.eta for PDx-Pop"
  :type 'boolean
  :group 'esn-pdx
  :group 'esn-$table-options
  )
(defcustom esn-pdx-gen-par-table 't
  "* Generate table.par for PDx-Pop"
  :type 'boolean
  :group 'esn-pdx
  :group 'esn-$table-options
  )



(defgroup esn-plt nil
  "* Esn PLT options"
  :group 'esn-mode
  )


(defcustom esn-plt-add-wide-to-data 't
  "* When using PLT tools, put WIDE keyword in $DATA.  May affect inputing of $DATA for PLT tools"
  :type 'boolean
  :group 'esn-plt
  :group 'esn-$data-options
  :group 'esn-changes-on-save
  )

(defcustom esn-plt-use-msfo-outputfile 't
  "* Uses PLT's convention of MSFO=msfo.outputfile"
  :type 'boolean
  :group 'esn-plt
  :group 'esn-$estimate-options
  )
(defcustom esn-plt-win-open nil
  "* Open PLT tools automation file instead of launching PLT tools to open the file under windows. Requires w32-browser"
  :type 'boolean
  :group 'esn-plt
  :group 'esn-windows
  )

(defcustom esn-plt-plta-quit nil
  "* Defines if PLT tools should quit after running the automation script."
  :type 'boolean
  :group 'esn-plt
  )

(defcustom esn-plt-gen-xpose nil
  "* Generate Xpose-type tables as well as PLT type tables."
  :type 'boolean
  :group 'esn-plt
  :group 'esn-xpose
  )
(defcustom esn-plt-gen-xpose-r-script nil
  "* Generate Xpose R-script for PLT tools"
  :type 'boolean
  :group 'esn-plt
  :group 'esn-xpose
  )
(defcustom esn-plt-gen-xpose-plt nil
  "* Generate Xpose graphics from PLT tables"
  :type 'boolean
  :group 'esn-plt
  :group 'esn-xpose
  )
(defcustom esn-plt-gen-one-table nil
  "* Generate one table for PLT tools as well as the PLT type tables."
  :type 'boolean
  :group 'esn-plt
  :group 'esn-single-table
  )

(defcustom esn-plt-exec-type 0
  "Choose what type of execution is done by default"
  :type '(choice (const :tag "NONMEM + Graphics" 0)
                 (const :tag "NONMEM only" 1)
                 (const :tag "Graphics only" 2)
                 )
  :group 'esn-plt
  :group 'esn-exec
  )
(defcustom esn-plt-rm-from-label '(
                                   "is"
                                   "applies"
                                   "appies"
                                   "to"
                                   )
  "* A list of words to remove from THETA/OMEGA/SIGMA labels to get Descriptors, and corresponding parameters"
  :type '(repeat
          (string :tag "Word")
          )
  :group 'esn-plt
  :group 'esn-extended
  )
(defcustom esn-plt-use-labels nil
  "* Defines if the initial PLT graphics script will use the
variable labels as a description or figure out what the label
should be based on name"
  :type 'boolean
  :group 'esn-plt
  )


(defcustom esn-mode-use-plt nil
  "* Defines if EsN uses PLT tools or not.  Requires control
stream to be in a WORK type directory.  Cannot use Xpose at the
same time as PLT Tools."
  :type 'boolean
  :group 'esn-plt
  )
(defcustom esn-plt-gen-graphics nil
  "* Generate a Graphics script when submitting to PLT tools"
  :type 'boolean
  :group 'esn-plt
  )
(defcustom esn-plt-use-userscript nil
  "* Use EsN's user-script to generate additional items"
  :type 'boolean
  :group 'esn-plt
  )
(defcustom esn-plt-age-covs '(
                              "AGE"
                              )
  "* Defines the assumed age covariates"
  :type '(repeat
          (string :tag "Age Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-race-covs '(
                               "RACE"
                               "RCEN"
                               )
  "* Defines the assumed race covariates"
  :type '(repeat
          (string :tag "Race Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-gender-covs '(
                                 "SEX"
                                 "SEXF"
                                 "SEXM"
                                 "GEND"
                                 "GNDR"
                                 "GNDF"
                                 "GNDM"
                                 )
  "* Defines the assumed gender covariates"
  :type '(repeat
          (string :tag "Gender Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-weight-covs '(
                                 "WT"
                                 "WTKG"
                                 "WTLB"
                                 "WGHT"
                                 )
  "* Defines the assumed weight covariates"
  :type '(repeat
          (string :tag "Weight Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-height-covs '(
                                 "HT"
                                 "HTCM"
                                 "HTM"
                                 "HTIN"
                                 "HTFT"
                                 "HGHT"
                                 )
  "* Defines the assumed Height covariates"
  :type '(repeat
          (string :tag "Height Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-dose-covs '(
                               "DOSE"
                               "DSE"
                               "DSGR"
                               "CTDS"
                               )
  "* Defines the assumed Dose covariates"
  :type '(repeat
          (string :tag "Dose Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-fed-covs '(
                              "FED"
                              "FAST"
                              )
  "* Defines the assumed fed/fasted covariates."
  :type '(repeat
          (string :tag "Fed/Fasted Variable:")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-formulation-covs '(
                                      "FORM"
                                      "FRML"
                                      )
  "* Defines the assumed formulation covariates."
  :type '(repeat
          (string :tag "Formulation Variable:")
          )
  :group 'esn-plt-tables
  )

(defcustom esn-plt-no-firstrecords '(
                                     )
  "* Defines the variables not to be automatically added to
Firstrecords.txt.  Disables prompting for these variables."
  :type '(repeat
          (string :tag "Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-firstrecords '(
                                  "ID"
                                  )
  "* Defines the variables required for the FirstRecords.txt table in addition to those generated in a Xpose-type patab"
  :type '(repeat
          (string :tag "Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-no-allrecords '(
                                   )
  "* Defines the variables not to be automatically added to
AllRecords.txt.  Disables prompting for these variables."
  :type '(repeat
          (string :tag "Variable: ")
          )
  :group 'esn-plt-tables
  )
(defcustom esn-plt-allrecords '("ID"
                                "TIME"
                                "IPRED"
                                "IPRE"
                                "IPRD"

                                "OPRED"
                                "OPRE"
                                "OPRD"

                                "VPRED"
                                "VPRE"
                                "VPRD"

                                "EVID"
                                "MDV"
                                "AMT"
                                "RATE"
                                "IRES"
                                "RES"
                                "WRES"
                                "IWRES"
                                "IWRE"
                                "PRED"
                                "DV"
                                "MDV"
                                "CMT"
                                "TAD"
                                "TSLD"
                                "OCC"
                                )
  "* Defines the variables required for the AllRecords.txt table"
  :type '(repeat
          (string :tag "Variable: ")
          )
  :group 'esn-plt-tables
  )

(defgroup esn-pirana nil
  "* Esn Pirana options"
  :group 'esn-mode
  )

(defcustom esn-mode-use-pirana nil
  "* Defines if Pirana assumptions for control streams is used
   (different header)"
  :type 'boolean
  :group 'esn-pirana
  )

(defcustom esn-pirana-exe
  (if esn-w32
      (progn
        (if (file-exists-p
             "c:\\Program Files\\Pirana\\Pirana.exe")
            "c:\\Program Files\\Pirana\\Pirana.exe"
          (if (file-exists-p "c:\\Pirana\\Pirana.exe")
              "c:\\Pirana\\Pirana.exe"
            ""
            )
          )
        )
    )
  "The string that defines the location of Pirana"
  :type 'file
  :group 'esn-pirana
  :group 'esn-exec
  )

(defgroup esn-psn nil
  "* EsN PsN options"
  :group 'esn-mode)

(defgroup esn-psn-bootstrap nil
  "PsN options for Bootstrapping."
  :group 'esn-psn)

(defcustom esn-mode-psn-boostrap-command "bootstrap"
  "PsN bootstrap command."
  :type 'string
  :group 'esn-psn-bootstrap)

(defcustom esn-mode-psn-bs-bca nil
  "Use BCa bootstrap method."
  :type 'boolean
  :group 'esn-psn-bootstrap)

(defcustom esn-bs-covariance-step-successful-limit 80
  "Limit for ratio of runs with covarince to runs without a covariance to list a warning about during summary."
  :type 'integer
  :group 'esn-psn-bootstrap)

(defcustom esn-bs-covariance-step-warnings-limit 20
  "Limit for ratio of runs with covarinace warnings, lists warning if above this value."
  :type 'integer
  :group 'esn-psn-bootstrap)


(defcustom esn-bs-estimate-near-boundary-limit 20
  "Limit for ratio of runs with estimates near the boundary list a warning about during summary."
  :type 'integer
  :group 'esn-psn-bootstrap)

(defcustom esn-bs-estimate-successful-limit 80
  "Limit for ratio of successful minization, lists warning if above this value."
  :type 'integer
  :group 'esn-psn-bootstrap)

(defcustom esn-bs-mplots nil
  "Create matlab scripts for making various plots of the result."
  :type 'boolean
  :group 'esn-psn-bootstrap)

(defcustom esn-bs-rplots 't
  "Create R scripts for making various plots of the result."
  :type 'boolean
  :group 'esn-psn-bootstrap)

(defcustom esn-skip-covariance-step-terminated nil
  "The bootstrap will skip all samples wehre the NONMEM run terminated the covariance step."
  :type 'boolean
  :group 'esn-psn-bootstrap)

(defcustom esn-skip-estimate-near-boundary nil
  "The bootstrap will skip all samples where the NONMEM run had estimates near its boundary."
  :type 'boolean
  :group 'esn-psn-bootstrap)

(defcustom esn-skip-minimization-terminated nil
  "With this option enabled, the bootstrap wil skip all samples where teh NONMEM run signal that some estimates are near its boundary."
  :type 'boolean
  :group 'esn-psn-bootstrap)

(defcustom esn-skip-with-covstep-warnings nil
  "With this option enabled, the bootstrap will skipp all samples where the NONMEM run had wranigns from the covariance step."
  :type 'boolean
  :group 'esn-psn-bootstrap
  )

(defgroup esn-psn-cdd nil
  "PsN options for Case Deletion Diagnostics."
  :group 'esn-psn)

(defcustom esn-mode-psn-cdd-command "cdd"
  "PsN Case Deletion Diagonistics command."
  :type 'string
  :group 'esn-psn-cdd)
(defcustom esn-mode-psn-cdd-rplots 't
  "Generate R Scripts for making plots of results."
  :type 'boolean
  :group 'esn-psn-cdd)

(defcustom esn-mode-psn-cdd-use-random-instead-of-consecutive nil
  "Use the random selection method instead of consecutive drawing from the datafile."
  :type 'boolean
  :group 'esn-psn-cdd)

(defcustom esn-mode-psn-cdd-xv 't
  "Use the cross-validation feature."
  :type 'boolean
  :group 'esn-psn-cdd)


(defgroup esn-psn-execute nil
  "Psn options for Submitting NONMEM jobs."
  :group 'esn-psn)

(defcustom esn-mode-psn-maxevals 9999
  "Defines the Maxevals that Perl Speaks NONMEM will handle (through using a MSF file."
  :type 'integer
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-retries 6
  "Defines the number of retries for a NONMEM run."
  :type 'integer
  :group 'esn-psn-execute
  )
(defcustom esn-mode-psn-tweak-inits nil
  "Defines if initail values are tweaked and then rerun with a unsuccessful NONMEM run."
  :type 'boolean
  :group 'esn-psn-execute)
(defcustom esn-mode-psn-picky nil
  "Defines if PsN will regard any of the following conditions a signal for rerunning:
Algorithmically Singular R or S matrix.
Parameters Estimate near its boundary."
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-ex-summarize 't
  "Defines if PsN will summarize output for each model run."
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-clean nil
  "Remove Directory/Archive after submitting job."
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-compress nil
  "Compress directory with tar and gzip (if available) after completion."
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-drop-dropped 't
  "PsN dropps the columns that are drop for INTERNAL PsN usage."
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-quick-summary nil
  "Prints the ojbective function and minimization message for each NONMEM run."
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-calculate-shrinkage nil
  "Calculates the shrinkage for the run?"
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-calculate-cwres nil
  "Calculates the CWRES for the run?"
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-wrap-data nil
  "Use Perl Speaks NONMEM to wrap data with a CONT column."
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-verbose nil
  "Be more verbose with Perl Speaks NONMEM execution."
  :type 'boolean
  :group 'esn-psn-execute)

(defcustom esn-mode-psn-sde nil
  "Defines if Stochastic Differntial Equations are used in the model."
  :type 'boolean
  :group 'esn-psn-execute)
(defcustom esn-mode-psn-execute-command "execute"
  "Defines the execute command for Perl Speaks NONMEM."
  :type 'string
  :group 'esn-psn-execute)

(defgroup esn-psn-llp nil
  "Psn options for Log Liklihood Profiling."
  :group 'esn-psn)

(defcustom esn-mode-psn-llp-command "llp"
  "Defines the log liklihood otion for Perl Speaks NONMEM."
  :type 'string
  :group 'esn-psn-llp)

(defcustom esn-mode-psn-llp-max-iterations 10
  "Defines the maximum number of search iterations for each interval limit.
If the upper limit for a parameter after max_iteration number of guesses it terminates."
  :type 'integer
  :group 'esn-psn-llp)
(defcustom esn-mode-psn-llp-mplots nil
  "Create matlab scripts for making plots of the results."
  :type 'boolean
  :group 'esn-psn-llp)
(defcustom esn-mode-psn-llp-rplots 't
  "Create R scripts for making plots of the results."
  :type 'boolean
  :group 'esn-psn-llp)

(defcustom esn-mode-psn-normq-number 1.96
  "Represents the normal Q value for the confidence interval.  The default is 1.96, which translates to a 95% confidence interval."
  :type 'number
  :group 'esn-psn-llp)

(defcustom esn-mode-psn-objv-increase-number 3.84
  "Represents the objective function increase associated with desired confidence interval. (Default for 95% Confidence interval is 3.84)"
  :type 'number
  :group 'esn-psn-llp)

(defcustom esn-mode-psn-sumo-command "sumo"
  "Defines the command for data summarization."
  :type 'string
  :group 'esn-psn)

(defcustom esn-mode-psn-vpc-command "vpc"
  "Defines the command for the visual predictive check (VPC)"
  :type 'string
  :group 'esn-psn)
(defcustom esn-mode-psn-npc-command "npc"
  "Defines the command for the numerical predictive check (NPC)"
  :type 'string
  :group 'esn-psn)


(defgroup esn-header-pdx nil
  "* EsN PDx pop header options"
  :group 'esn-header
  :group 'esn-pdx
  )
(defgroup esn-header-plt nil
  "* EsN PLT tools header options"
  :group 'esn-header
  :group 'esn-plt
  )
(defgroup esn-header-census nil
  "* EsN Census header options"
  :group 'esn-header
  :group 'esn-census
  )
(defgroup esn-header-pirana nil
  "* EsN Pirana header options"
  :group 'esn-header
  :group 'esn-pirana
  )
(defgroup esn-header-def nil
  "* EsN default header options"
  :group 'esn-header
  )

(defgroup esn-display nil
  "* EsN display Options"
  :group 'esn-mode
  )
(defgroup esn-font-lock nil
  "* EsN fontlock Options"
  :group 'esn-display
  )
(defcustom esn-adaptive-font-lock 't
  "* When non-nil, font lock of options are based on context (takes longer).
When nil  all options are conglomerated and then highlighted wherevert they are found in the buffer.
"
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-unknown-records 't
  "* When non-nil, font lock unknown records (based on NONMEM version selected)"
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-known-option-values 't
  "* When non-nil, font lock known option values (Based on NONMEM version selected)"
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-input-items-over-4 't
  "* When using NONMEM 6 and below, highlight input items over 4 characters in length?"
  :type 'boolean
  :group 'esn-font-lock
  :group 'esn-$input-options
  )

(defcustom esn-highlight-input-items-over-data 't
  "* When using CSV files, highlight $INPUT data items that are OVER the number of data items required."
  :type 'boolean
  :group 'esn-font-lock
  :group 'esn-$input-options
  )
(defcustom esn-highlight-operators 't
  "* Highlight operators"
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-lhs 't
  "* Highlight known/reserved variables (like DADT(1)) on the left-handed side of an equation in abbreviated code (like $DES)."
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-rhs 't
  "* Highlight known/reserved variables (like THETA(1)) on the right-handed side of an equation in abbreviated code (like $PK)."
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-lhs-warning 't
  "* Highlight variables that should be assigned (Like THETA(1)) in $PK"
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-abbrev-forbidden 't
  "* Highlight forbidden variables in abbreviated code."
  :type 'boolean
  :group 'esn-font-lock
  )
(defcustom esn-highlight-bad-mu 't
  "* Highlights MU_# variables that do not match the eta.  For example:

MU_1 = THETA(1)
CL = MU_1+ETA(2)

MU_1 doesn't match ETA(2), so this is highlighted"
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-abbrev-undefined  't
  "* Highlight undefined variables that are used abbreviated code."
  :type 'boolean
  :group 'esn-font-lock
  )

(defcustom esn-highlight-known-vars 't
  "* Highlight known variables that are used in abbreviated code."
  :type 'boolean
  :group 'esn-font-lock
  )

(defgroup esn-symbols nil
  "* EsN symbol display options"
  :group 'esn-display
  )

(defcustom esn-use-symbols nil
  "* Use symbols when displaying buffers for ESN."
  :type 'boolean
  :group 'esn-symbols
  )
(defcustom esn-symbols-dadt 't
  "* Use Partial notation for DADT(1)"
  :type 'boolean
  :group 'esn-symbols
  )
(defcustom esn-superscripts 't
  "* Use superscripts for X**# where # is a numeric value"
  :type 'boolean
  :group 'esn-symbols
  )
(defcustom esn-subscripts 't
  "* Use subscripts for A1 and A_1 (but not A_0(#))"
  :type 'boolean
  :group 'esn-symbols
  )
(defcustom esn-operators 't
  "* Use symbols for NONMEM operators"
  :type 'boolean
  :group 'esn-symbols
  )
(defcustom esn-symbol-words 't
  "Use symbols for words such as THETA(1) (w/ subscript) and LAMBDA"
  :type 'booelan
  :group 'esn-symbols
  )
(defcustom esn-symbols-a_0 't
  "Use subscripts for A_0(#), P(#), A(#), etc."
  :type 'boolean
  :group 'esn-symbols
  )

(defcustom esn-symbols-expanded 't
  "* Use \"expanded\" symbols.  Therefore instead of A_# becoming A#, it keeps the _.  Also keeps parentheses."
  :type 'boolean
  :group 'esn-symbols
  )

(defgroup esn-hidden nil
  "*EsN hidden text options"
  :group 'esn-display
  )


(defgroup esn-programs nil
  "* EsN program interfaces"
  :group 'esn-mode
  )

(defgroup esn-indenting nil
  "* EsN indenting options"
  :group 'esn-mode
  )


(defcustom esn-mode-auto-indent 't
  "* If not nil, ESN-mode indents according to the mode."
  :type 'boolean
  :group 'esn-indenting)

(defcustom esn-mode-indent-amount 2
  "* The amount to indent for records not indentend to record length and for IF ENDIF sorts of clauses."
  :type 'integer
  :group 'esn-indenting
  )
(defcustom esn-mode-indent-keyword-regexp "\\(?:\\<IF\\>.+\\<THEN\\>\\|\\<\\(DO\\|FUNCTION\\|SUBROUTINE\\)\\>\\)"
  "* The regular expression for an increase in indentation."
  :type 'regexp
  :group 'esn-indenting)


(defcustom esn-mode-deindent-keyword-regexp "\\<END\\(IF\\|DO\\)\\>"
  "* The regular expression to decrease indentation."
  :type 'regexp
  :group 'esn-indenting
  )
(defcustom esn-mode-deindent-indent-keyword-regexp "\\<ELSE\\>"
  "* The regular expression that decreases indentation for current line, but increases indentation afterward."
  :type 'regexp
  :group 'esn-indenting
  )


(defcustom esn-mode-auto-indent-force-zero-indent '(
                                                    "TAB"
                                                    "PRO"
                                                    )
  "* A list of records to force zero indentation.  Just put the first three leters of the record."
  :type '(repeat
          (string :tag "The 3 char $REC")
          )
  :group 'esn-indenting
  :group 'esn-records
  )

(defgroup esn-naming nil
  "* EsN file naming conventions"
  :group 'esn-mode
  )
(defgroup esn-extended nil
  "* Extended control stream options"
  :group 'esn-mode
  )


(defcustom esn-wfn-mu-var 't
  "* Defines if MU translation should be performed"
  :type 'boolean
  :group 'esn-extended
  )

(defcustom esn-wfn-var-label-skip-tos 't
  "* Defines if Variable labels such as ; THETA(1) - are skipped when figuring out what the variable names."
  :type 'boolean
  :group 'esn-extended
  )

(defcustom esn-wfn-var-label-skip-afp 't
  "* Defines if Variable labels such as \";[P]\", \";[F]\"
  and ;\"[A]\" - are skipped when figuring out what the variable
  names."
  :type 'boolean
  :group 'esn-extended
  )


(defcustom esn-wfn-extended nil
  "Extended control streams save to normal control streams, and open as extended control streams."
  :type 'boolean
  :group 'esn-extended
  )
(defcustom esn-wfn-color-extended 't
  "* Defines if the extended control stream variables are highlighted."
  :type 'boolean
  :group 'esn-extended
  :group 'esn-font-lock
  )
(defcustom esn-wfn-extended-zero 't
  "* Extended variable names are used in the ZERO=(x) statment for hybrid estimation models."
  :type 'boolean
  :group 'esn-extended
  )
(defcustom esn-wfn-prefer-lc 't
  "Extended control streams are opened in lower case instead of upper case."
  :type 'boolean
  :group 'esn-extended
  )
(defcustom esn-wfn-caps 't
  "Save a NONMEM-capitalized buffer."
  :type 'boolean
  :group 'esn-extended
  :group 'esn-changes-on-save
  )
(defcustom esn-update-wfn-labels-when-editing nil 
  "When typing labels during editing THETA SIGMA and OMEGA
records, keep labels in model blocks up to date?  Requires
`esn-wfn-extended' to be true."
  :type 'boolean
  :group 'esn-extended
  :group 'esn-changes-while-editing
  )


(defgroup esn-wfn nil
  "WFN options for ESN-mode"
  :group 'esn-mode)

(defcustom esn-mode-wfn-bat (if (file-exists-p "c:/nmvi/wfn6/bin/wfn.bat")
                                "c:/nmvi/wfn6/bin/wfn.bat"
                              (if (file-exists-p "c:/nm7/wfn7/bin/wfn.bat")
                                  "c:/nm7/wfn7/bin/wfn.bat"
                                ""
                                ))
  "* Where the Wings for NONMEM batch file is run from."
  :type 'file
  :group 'esn-wfn
  :group 'esn-exec
  )

(defcustom esn-mode-wfn-nmgo "nmgo"
  "Command to submit NONMEM jobs with WFL"
  :group 'esn-wfn
  )

(defcustom esn-mode-wfn-nmbs "nmbs"
  "Command for bootstrapping run with WFL"
  :group 'esn-wfn
  )

(defcustom esn-mode-wfn-nmrt "nmrt"
  "Command for randomization test with WFL"
  :group 'esn-wfn
  )


(defgroup esn-windows nil
  "* EsN on Windows options"
  :group 'esn-mode
  )

(defcustom esn-mode-tab-completion-browse-files-w32 't
  "* If not nil, EsN-mode browses for files through windows dialog when dlgopen is enabled."
  :type 'boolean
  :group 'esn-windows)


(defgroup esn-mode-formatting nil
  "Options for Formatting"
  :group 'esn-mode
  )
(defcustom esn-sub-begin "@"
  "* Defines how subroutine comments begin.  The traditional ;; is replace with ; plus this value."
  :type 'string
  :group 'esn-mode-formatting
  :group 'esn-$subroutines-options
  )
(defcustom esn-sub-end "@"
  "* Defines how subroutine comments begin.  The traditional ;; is replaced with this value plus ;."
  :type 'string
  :group 'esn-mode-formatting
  :group 'esn-$subroutines-options
  )

(defgroup esn-help nil
  "* EsN help options"
  :group 'esn-mode
  )

(defcustom esn-help-wrap-to 100
  "* Wrap to this number of characters"
  :group 'esn-help)

(defgroup esn-link nil
  "PLT options for ESN-mode"
  :group 'esn-mode
  )
(defcustom esn-use-hyperlinks 't
  "Use Hyperlinks within Emacs Speaks NONMEM"
  :type 'boolean
  :group 'esn-link
  )

(defcustom esn-link-outfiles 't
  "Link to output files"
  :type 'boolean
  :group 'esn-link
  :group 'esn-$table-options
  )
(defcustom esn-link-msfo 't
  "Link to MSFO files"
  :type 'boolean
  :group 'esn-link
  :group 'esn-$estimate-options
  :group 'esn-$nonparametric-options
  )
(defcustom esn-link-ref-models 't
  "Link to Reference models defined by Pirana or Census"
  :type 'boolean
  :group 'esn-link
  :group 'esn-pirana
  :group 'esn-census
  )
(defcustom esn-link-include-files 't
  "Link to Include files defined by ;INCLUDE= or ;INCLUDE:"
  :type 'boolean
  :group 'esn-link
  :group 'esn-pirana
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random Variables
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup esn-rv nil
  "* Random variable specifications"
  :group 'esn-mode
  )
(defcustom esn-mode-rv-add "THETA(%s)"
  "* Random error used for W=x when additive error"
  :type 'string
  :group 'esn-rv
  )
(defcustom esn-mode-rv-prop "THETA(%s)*F"
  "* Random error used for W=x when proportional error"
  :type 'string
  :group 'esn-rv
  )

(defcustom esn-mode-rv-logn "THETA(%s)"
  "* Random error used for W=x when lognormal error"
  :type 'string
  :group 'esn-rv
  )

(defcustom esn-mode-rv-add-prop "SQRT(THETA(%s)**2+THETA(%s)**2*F**2)"
  "* Random error used for W=x when add+prop error.  First theta is additive, second theta proportional."
  :type 'string
  :group 'esn-rv
  )

(defcustom esn-mode-rv-logn-tini "       (0, %s) ; Residual Lognormal SD"
  "* Random error used for W=x when add+prop error.  First theta is additive, second theta proportional."
  :type 'string
  :group 'esn-rv
  )

(defcustom esn-mode-rv-add-tini "       (0, %s) ; Residual Additive SD"
  "* Random error used for W=x when add+prop error.  First theta is additive, second theta proportional."
  :type 'string
  :group 'esn-rv
  )
(defcustom esn-mode-rv-prop-tini "       (0, %s) ; Residual Proportional SD"
  "* Random error used for W=x when add+prop error.  First theta is additive, second theta proportional."
  :type 'string
  :group 'esn-rv
  )
(defcustom esn-mode-rv-add-prop-tini "       (0, %s) ; Residual Additive SD\n       (0, %s) ; Residual Proportional SD"
  "* Random error used for W=x when add+prop error.  First theta is additive, second theta proportional."
  :type 'string
  :group 'esn-rv
  )

(defgroup esn-xmind nil
  "Xmind options for ESN-mode"
  :group 'esn-mode
  )
(defcustom esn-xmind-exe
  (if (and esn-w32 (w32-short-file-name "C:/Program Files/XMind/xmind.exe")
           (file-exists-p
            (w32-short-file-name "C:/Program Files/XMind/xmind.exe")
            ))
      (w32-short-file-name "C:/Program Files/XMind/xmind.exe")
    ""
    )
  "* Defines the program location of Xmind"
  :type 'file
  :group 'esn-xmind
  :group 'esn-exec
  )
(defcustom esn-xmind-update-topic-name 't
  "* Defines if the topic name is updated after a file save."
  :type 'boolean
  :group 'esn-xmind
  )
(defcustom esn-xmind-update-xmind-comment 't
  "* Defines if the Parent topic is updated on file open and file save."
  :type 'boolean
  :group 'esn-xmind
  )

(defcustom esn-xmind-default-map "_project.xmind"
  "* Defines the default Xmind map."
  :type 'string
  :group 'esn-mode
  )
(defcustom esn-mode-use-xmind nil
  "* Defines if Emacs Speaks NONMEM will create/update a Xmind mind map."
  :type 'string
  :group 'esn-mode
  )

(defcustom esn-xmind-create-new-topic-on-move 't
  "* Creates a new topic if a Xmind topic string is changed to a topic that doesn't yet exist in the project map."
  :type 'boolean
  :group 'esn-xmind
  )

(defgroup esn-mode-xpose nil
  "* Xpose options for ESN-mode"
  :group 'esn-mode
  )
(defgroup esn-xpose-tables nil
  "* Xpose table options for ESN-mode"
  :group 'esn-mode-xpose
  :group 'esn-automatically-fill-in-tables
  )

(defcustom esn-mode-create-xpose-summary-script nil
  "* Boolean to define if the summary script is created upon save."
  :type 'boolean
  :group 'esn-mode-xpose
  :group 'esn-code-generation
  )
(defcustom esn-mode-use-macro 't
  "* Boolean to define if the summary script should include
  macros that are run to password protect, space the tables
  appropriately, update the fields in the document, and save as a
  .doc file."
  :type 'boolean
  :group 'esn-mode-xpose
  )
(defcustom esn-mode-output-data 't
  "* Boolean that defines if the summary script should output a csv file of the merged data (after all manipulations have been done)."
  :type 'boolean
  :group 'esn-mode-xpose
  :group 'esn-code-generation
  )
(defcustom esn-mode-log-equal 1e-3
  "* Tolerance to say that log(x)=DV to determine if the current control stream is on a log scale."
  :type 'number
  :group 'esn-mode-xpose
  :group 'esn-code-generation
  :group 'esn-rv
  )
(defcustom esn-mode-xpose-summary-document-comparisons
  (quote (
          ("Model Comparison"
           (
                                        ;           "theta.table.cmp"
                                        ;           "eta.table.cmp"
                                        ;           "eps.table.cmp"
                                        ;           "cor.table"
                                        ;           "cmp.notes"
            "basic.model.comp"
            "basic.model.comp.cwres"
            "add.model.comp"
                                        ;           "absval.dcwres.vs.cov.model.comp"
                                        ;           "absval.dipred.vs.cov.model.comp"
                                        ;           "absval.diwres.vs.cov.model.comp"
                                        ;           "absval.dpred.vs.cov.model.comp"
                                        ;           "absval.dwres.vs.cov.model.comp"
            )
           )
          ))
  "* This defines the structure of the Xpose summary document that can be generted for comparison plots."
  :type '(repeat (list
                  (string :tag "Section Title")
                  (repeat (string :tag "Xpose/ESN function call"))
                  )
                 )
  :group 'esn-mode-xpose
  :group 'esn-code-generation
  )
(defcustom esn-mode-xpose-summary-document
  (quote (
          ("Model Overview" (
                                        ;                      "overview"
                                        ;                       "runsum"
                             "theta.table"
                             "eta.table"
                             "eps.table"
                             "cor.table"
                             "errors"
                             "cov.summary"
                             "par.summary"))
                                        ;    ("Basic Goodness Of Fit Plots" (
                                        ;                                    "dv.vs.idv.pred"
                                        ;                                    "dv.vs.idv.pred.log"
                                        ;                                   "dv.vs.pred.ipred"
                                        ;                                   "dv.vs.pred.ipred.log"))
                                        ;    ("Structural model diagnostics" (
                                        ;                                    "dv.vs.pred.by.idv"
                                        ;                                    "dv.vs.ipred.by.idv"
                                        ;
                                        ;                                    "dv.vs.pred.by.idv.log"
                                        ;                                    "dv.vs.ipred.by.idv.log"
                                        ;
                                        ;                                    "dv.vs.pred.by.tsld"
                                        ;                                    "dv.vs.ipred.by.tsld"
                                        ;
                                        ;                                    "dv.vs.pred.by.tsld.log"
                                        ;                                    "dv.vs.ipred.by.tsld.log"
                                        ;
                                        ;                                    "dv.vs.pred.by.tad"
                                        ;                                    "dv.vs.ipred.by.tad"
                                        ;
                                        ;                                    "dv.vs.pred.by.tad.log"
                                        ;                                    "dv.vs.ipred.by.tad.log"
                                        ;
                                        ;                                    "cwres.vs.idv"
                                        ;                                    "cwres.vs.idv.bw"
                                        ;
                                        ;                                    "cwres.vs.tsld"
                                        ;                                    "cwres.vs.tsld.bw"
                                        ;
                                        ;                                    "cwres.vs.tad"
                                        ;                                    "cwres.vs.tad.bw"
                                        ;
                                        ;                                    "cwres.vs.pred"
                                        ;                                    "cwres.vs.pred.bw"
                                        ;
                                        ;                                    "wres.vs.idv"
                                        ;                                    "wres.vs.idv.bw"
                                        ;
                                        ;                                    "wres.vs.tsld"
                                        ;                                    "wres.vs.tsld.bw"
                                        ;
                                        ;                                    "wres.vs.tad"
                                        ;                                    "wres.vs.tad.bw"
                                        ;
                                        ;
                                        ;                                    "wres.vs.pred"
                                        ;                                    "wres.vs.pred.bw"
                                        ;
                                        ;                                    "dv.vs.pred.by.cov"
                                        ;                                    "dv.vs.ipred.by.cov"
                                        ;
                                        ;                                    "dv.vs.pred.by.cov.log"
                                        ;                                    "dv.vs.ipred.by.cov.log"
                                        ;                                    ))
                                        ;    ("Residual model diagnostics" (
                                        ;                                  "wres.dist.hist"
                                        ;                                  "wres.dist.qq"
                                        ;                                  "ind.plots.wres.hist"
                                        ;                                  "ind.plots.wres.qq"
                                        ;                                  "abs.wres.vs.pred"
                                        ;                                  "cwres.dist.hist"
                                        ;                                  "cwres.dist.qq"
                                        ;                                  "ind.plots.cwres.hist"
                                        ;                                  "ind.plots.cwres.qq"
                                        ;                                  "abs.cwres.vs.pred"
                                        ;
                                        ;                                  "abs.iwres.wres.vs.ipred.pred"
                                        ;                                  "abs.iwres.vs.ipred"
                                        ;                                  "abs.wres.vs.cov.bw"
                                        ;                                  "abs.wres.vs.pred.by.cov"
                                        ;                                  "abs.cwres.vs.cov.bw"
                                        ;                                  "abs.cwres.vs.pred.by.cov"
                                        ;                                  "abs.iwres.vs.ipred.by.cov"
                                        ;                                  "autocorr.wres"
                                        ;                                  "autocorr.cwres"
                                        ;                                  ))
                                        ;    ("Delta Plots" ("delta.vs.cov"
                                        ;                   "param.vs.cov"
                                        ;                   ))
                                        ;    ("Parameter Plots" ("parm.qq"
                                        ;                  "parm.hist"
                                        ;                  "parm.splom"
                                        ;                  "ranpar.qq"
                                        ;                  "ranpar.hist"
                                        ;                  "ranpar.splom"
                                        ;                 ))
                                        ;    ("Covariate Plots" (
                                        ;                  "cov.splom"
                                        ;                  "param.vs.cov"
                                        ;                  "wres.vs.cov"
                                        ;                  ) )
          ("Individual Plots" ("ind.plots.log" "ind.plots"))
                                        ;    ))
          ))
  
  "* This defines the structure of the Xpose summary document that can be generted."
  :type '(repeat (list
                  (string :tag "Section Title")
                  (repeat (string :tag "Xpose/ESN function call"))
                  )
                 )
  :group 'esn-mode-xpose
  :group 'esn-code-generation
  )
(defcustom esn-xpose-generate-tables nil
  "Generate/Update Xpose tables on save."
  :type 'boolean
  :group 'esn-xpose-tables
  :group 'esn-xpose
  )
(defcustom esn-plt-generate-tables nil
  "Generate/Update PLT tables on save."
  :type 'boolean
  :group 'esn-plt
  )
(defcustom esn-pdx-generate-tables nil
  "Generate/Update PDx tables on save."
  :type 'boolean
  :group 'esn-pdx
  )
(defcustom esn-generate-one-table nil
  "* Generate One table"
  :type 'boolean
  :group 'esn-mode
  )
(defcustom esn-xpose nil
  "* Defines if Xpose options are implemented."
  :type 'boolean
  :group 'esn-mode-xpose
  )
(defcustom esn-xpose-default-extension ".mod"
  "* Defines the default extension for Xpose files."
  :type 'string
  :group 'esn-mode-xpose
  :group 'esn-naming
  )
(defcustom esn-xpose-default-output ".lst"
  "* Defines teh default extension for Xpose listing files."
  :type 'string
  :group 'esn-mode-xpose
  :group 'esn-naming
  )

(defcustom esn-xpose-data-extension ""
  "* Defines the default output for Xpose table files."
  :type 'string
  :group 'esn-mode-xpose
  :group 'esn-naming
  )

(defcustom esn-xpose-data-sim-extension "sim"
  "* Defines the default output extension for Simulation table files."
  :type 'string
  :group 'esn-mode-xpose
  :group 'esn-naming
  )

(defcustom esn-xpose-choose-file-name nil
  "* Naming convention runX.mod"
  :type 'boolean
  :group 'esn-mode-xpose
  :group 'esn-naming
  )
(defcustom esn-xpose-choose-file-name-padding nil
  "* Naming convention runXXX.mod"
  :type 'boolean
  :group 'esn-mode-xpose
  :group 'esn-naming
  )

(defcustom esn-xpose-choose-file-name-no-run nil
  "* Naming convention X.mod"
  :type 'boolean
  :group 'esn-mode-xpose
  :group 'esn-naming
  )
(defcustom esn-xpose-choose-file-name-no-run-padding nil
  "* Naming convention XXX.mod"
  :type 'boolean
  :group 'esn-mode-xpose
  :group 'esn-naming
  )

(defcustom esn-xpose-tables-for-non-xpose-ctl-streams 't
  "Generates Xpose tables when the control stream doesn't match Xpose naming conventions."
  :type 'boolean
  :group 'esn-mode-xpose
  )
(defcustom esn-xpose-small-table-file-names nil
  "Generates small table file names for Xpose.  Instead of cotab, co, catab, ca, etc."
  :type 'boolean
  :group 'esn-mode-xpose
  :group 'esn-xpose-tables
  )
(defalias 'esn-xpose-insert-table-after 'esn-insert-table-after)
(defcustom esn-insert-table-after
  '(
    "INP"
    "PRE"
    "PRO"
    "PK"
    "THE"
    "THT"
    "OME"
    "SIG"
    "MSF"
    "COV"
    )
  "* Defines the records that should preceed the inserted xpose table. DO NOT include TAB EST NON or SIM."
  :type '(repeat
          (string :tag "3 Character $REC")
          )
  :group 'esn-xpose-tables
  :group 'esn-record-options
  )


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table count/split
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xpose options
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yas
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup esn-yas nil
  "* Yasnippet options for EsN"
  :group 'esn-mode
  )
(defcustom esn-yas-use-mu-referencing 't
  "* Adds mu-referencing to templates"
  :type 'boolean
  :group 'esn-yas
  )

(defcustom esn-yas-mu-referencing-type 2
  "* Choose the Mu referecing type"
  :type '(choice
          (const :tag "Use Mu referecing after TV is defined" 1)
          (const :tag "Use Mu referecing ASAP, THETA estimates actual parameter" 2)
          (const :tag "Use Mu referecing ASAP, THETA estimates may be transformed" 3)
          )
  :group 'esn-yas
  )
(defcustom esn-yas-omega-formatting " %s_BSV"
  "* String indicating the omega formatting.  The %s is replaced with the parameter name"
  :type 'string
  :group 'esn-yas
  )
(defcustom esn-yas-insert-theta-after
  '(
    "INP"
    "PRE"
    "PRO"
    "PK"
    )
  "* Defines the records that should preceed the inserted $THETA (if needed)"
  :type '(repeat
          (string :tag "3 Character $REC")
          )
  :group 'esn-yas
  :group 'esn-record-options
  )
(defgroup esn-iov nil
  "* IOV options"
  :group 'esn-mode)
(defcustom esn-iov-occ "\\<OC[0-9]+\\>"
  "* IOV occasion regular expression.  Currently assumes OC# are occasion variables."
  :type 'regexp
  :group 'esn-iov
  )
(defcustom esn-tab-complete-iov 't
  "* Tab Compete IOV#"
  :group 'esn-iov
  )
(defcustom esn-yas-insert-omega-after
  '(
    "INP"
    "PRE"
    "PRO"
    "PK"
    "THE"
    
    )
  "* Defines the records that should preceed the inserted $OMEGA (if needed)"
  :type '(repeat
          (string :tag "3 Character $REC")
          )
  :group 'esn-yas
  :group 'esn-$omega-options
  :group 'esn-iov
  :group 'esn-record-options
  )

(defcustom esn-eta-types '(
                           ("No Between Subject Error"
                            "%s  = %s"
                            ""
                            
                            ""
                            ""
                            ""
                            
                            ""
                            ""      ;
                            ""
                            (("";1
                              "" ;2
                              "" ;3
                              "" ;4
                              "" ;5
                              ))
                            ""
                            )
                           ("Lognormal" ;; Name
                            "%s  = %s*DEXP(ETA(%s))" ;; Expression, TV, no mu or mu after
                            "MU_%s = DLOG(%s)" ;; Expression mu after
                            
                            "MU_%s = DLOG(THETA(%s))" ; 1st Mu, Trans
                            "DEXP(MU_%s)" ;; 1st Mu, Trans TV
                            "%s = DEXP(MU_%s+ETA(%s))" ;; 1st Mu, Trans Actual
                            
                            "MU_%s = THETA(%s)" ; 1st Mu, No Trans
                            "DLOG(MU_%s)" ;; 1st Mu, No Trans TV
                            "%s  = DEXP(MU_%s+ETA(%s))" ;; 1st Mu, No Trans Actual
                            (("LOG(%s)" "0" "" "" "")) ;; Theta name, Trans
                            "[P]"
                            )
                           ("Additive" ;; Name
                            "%s  = %s+ETA(%s)" ;; Expression, TV, no mu or mu after
                            "MU_%s = %s" ;; Expression mu after
                            
                            "MU_%s = THETA(%s)" ; 1st Mu, Trans
                            "MU_%s" ;; 1st Mu, Trans TV
                            "%s  = MU_%s+ETA(%s)" ;; 1st Mu, Trans Actual
                            
                            "MU_%s = THETA(%s)" ; 1st Mu, No Trans
                            "MU_%s" ;; 1st Mu, No Trans TV
                            "%s  = MU_%s+ETA(%s)" ;; 1st Mu, No Trans Actual
                            (("%s" "" "" "" "")) ;; Theta name, Trans
                            "[A]"
                            )
                           ("Logit" ;; Name
                            "RTV%1% = DLOG(%2%/(1-%2%))\n TR%1% =RTV%1%+ETA(%3%)\n  %1%  = DEXP(TR%1%)/(1+DEXP(TR%1%))"
                            "MU_%s = R%s"
                            
                            "MU_%1% = DLOG(THETA(%2%)/(1-THETA(%2%)))"
                            "DEXP(MU_%1%)/(1+DEXP(MU_%1%))"
                            "%1%  = DEXP(MU_%2%+ETA(%3%))/(1+DEXP(MU_%2%+ETA(%3%)))"
                            
                            "MU_%s = THETA(%s)"
                            "DEXP(MU_%1%)/(1+DEXP(MU_%1%))"
                            "%1%  = DEXP(MU_%1%+ETA(%2%))/(1+DEXP(MU_%1%+ETA(%2%)))"
                            
                            (("Logit(%s)" "0" "1" "" ""))
                            "[N]"
                            )
                           ("Constrained Lognormal (Theta > C)"
                            "%s  = %s+THETA(%NEXT-THETA%)*DEXP(ETA(%s))" ;; Expression, TV, no mu or mu after
                            "MU_%s = DLOG(THETA(%NEXT-THETA%))" ;; Expression mu after ETA# TV
                            
                            "MU_%s = DLOG(THETA(%NEXT-THETA%))" ; 1st Mu, Trans
                            "THETA(%CURRENT-THETA%)" ;; 1st Mu, Trans TV
                            "%s = %TVCURRENT%+DEXP(MU_%s+ETA(%s))" ;; 1st Mu, Trans Actual
                            
                            "MU_%s = THETA(%NEXT-THETA%)" ; 1st Mu, No Trans
                            "THETA(%CURRENT-THETA%)" ;; 1st Mu, No Trans TV
                            "%s  = %TVCURRENT%+DEXP(MU_%s+ETA(%s))" ;; 1st Mu, No Trans Actual
                            (("%s" "$" "" "$" "")
                             ("Extra Theta for %s constraint" "0" "" "" "")
                             ) ;; Theta name, Trans
                            "[P]"
                            )
                           ("Proportional" ;; Name
                            "%s  = %s*(1+ETA(%s))" ;; Expression TV, no mu or mu after
                            ""
                            
                            ""
                            ""
                            ""
                            
                            ""
                            ""
                            ""
                            (("" "" "" "" ""))
                            "[P]"
                            )
                           )
  "* Between Subject Variability types.

There are three types known:

Type 1:

  Usual typical value rendering.  For example:

  TVCL = THETA(1)
  CL   = TVCL*DEXP(ETA(1))
  MU_1 = DLOG(TVCL)


Type 2:
  Render MU_x ASAP, and have THETA(x) estimate actual Cl, V, etc.  For example:

  MU_1 = DLOG(THETA(1))
  ...
  TVCL = DEXP(MU_1)
  CL   = DEXP(MU_1+ETA(1))

Type 3:
  Render MU_x ASAP, and have THETA(x) estimate transformed Cl, V, etc. For example:

  MU_1 = THETA(1)
  ...
  TVCL = DLOG(MU_1)
  CL   = DEXP(MU_1+ETA(1))

"
  :type '(repeat
          (list
           (string :tag "Between Subject Error Name") ;0
           (string :tag "Type 1 parameter (Par TVPar ETA#)") ;1
           (string :tag "Type 1 Mu (ETA# TV)")  ;2
           
           (string :tag "Type 2 MU (ETA# THETA#)") ;3
           (string :tag "Type 2 TV (ETA#)") ;4
           (string :tag "Type 2 parameter (Par ETA# ETA#)") ;5
           
           (string :tag "Type 3 Mu (ETA# THETA#)") ;6
           (string :tag "Type 3 TV (ETA#)") ;7
           (string :tag "Type 3 parameter (Par ETA# ETA#)") ;8
           (repeat (list
                    (string :tag "Type 3 parameter Label (Par)") ; 0
                    (string :tag "Normal Lower Bound") ; 1
                    (string :tag "Normal Upper Bound") ; 2
                    (string :tag "Transformed Lower Bound") ;3 
                    (string :tag "Transformed Upper Bound") ; 4
                    
                    )
                   )
           (string :tag "PDx pop error type ([A] [P] [N] or [F])") ; 10
           )
          )
  :group 'esn-yas
  )
(defvar esn-advan-trans-5
  '(
    ("K21" "Rate constant from compartment 2 to compartment 1")
    ("K31" "Rate constant from compartment 3 to compartment 1")
    ("K41" "Rate constant from compartment 4 to compartment 1")
    ("K51" "Rate constant from compartment 5 to compartment 1")
    ("K61" "Rate constant from compartment 6 to compartment 1")
    ("K71" "Rate constant from compartment 7 to compartment 1")
    ("K81" "Rate constant from compartment 8 to compartment 1")
    ("K91" "Rate constant from compartment 9 to compartment 1")
    ("K12" "Rate constant from compartment 1 to compartment 2")
    ("K32" "Rate constant from compartment 3 to compartment 2")
    ("K42" "Rate constant from compartment 4 to compartment 2")
    ("K52" "Rate constant from compartment 5 to compartment 2")
    ("K62" "Rate constant from compartment 6 to compartment 2")
    ("K72" "Rate constant from compartment 7 to compartment 2")
    ("K82" "Rate constant from compartment 8 to compartment 2")
    ("K92" "Rate constant from compartment 9 to compartment 2")
    ("K13" "Rate constant from compartment 1 to compartment 3")
    ("K23" "Rate constant from compartment 2 to compartment 3")
    ("K43" "Rate constant from compartment 4 to compartment 3")
    ("K53" "Rate constant from compartment 5 to compartment 3")
    ("K63" "Rate constant from compartment 6 to compartment 3")
    ("K73" "Rate constant from compartment 7 to compartment 3")
    ("K83" "Rate constant from compartment 8 to compartment 3")
    ("K93" "Rate constant from compartment 9 to compartment 3")
    ("K14" "Rate constant from compartment 1 to compartment 4")
    ("K24" "Rate constant from compartment 2 to compartment 4")
    ("K34" "Rate constant from compartment 3 to compartment 4")
    ("K54" "Rate constant from compartment 5 to compartment 4")
    ("K64" "Rate constant from compartment 6 to compartment 4")
    ("K74" "Rate constant from compartment 7 to compartment 4")
    ("K84" "Rate constant from compartment 8 to compartment 4")
    ("K94" "Rate constant from compartment 9 to compartment 4")
    ("K15" "Rate constant from compartment 1 to compartment 5")
    ("K25" "Rate constant from compartment 2 to compartment 5")
    ("K35" "Rate constant from compartment 3 to compartment 5")
    ("K45" "Rate constant from compartment 4 to compartment 5")
    ("K65" "Rate constant from compartment 6 to compartment 5")
    ("K75" "Rate constant from compartment 7 to compartment 5")
    ("K85" "Rate constant from compartment 8 to compartment 5")
    ("K95" "Rate constant from compartment 9 to compartment 5")
    ("K16" "Rate constant from compartment 1 to compartment 6")
    ("K26" "Rate constant from compartment 2 to compartment 6")
    ("K36" "Rate constant from compartment 3 to compartment 6")
    ("K46" "Rate constant from compartment 4 to compartment 6")
    ("K56" "Rate constant from compartment 5 to compartment 6")
    ("K76" "Rate constant from compartment 7 to compartment 6")
    ("K86" "Rate constant from compartment 8 to compartment 6")
    ("K96" "Rate constant from compartment 9 to compartment 6")
    ("K17" "Rate constant from compartment 1 to compartment 7")
    ("K27" "Rate constant from compartment 2 to compartment 7")
    ("K37" "Rate constant from compartment 3 to compartment 7")
    ("K47" "Rate constant from compartment 4 to compartment 7")
    ("K57" "Rate constant from compartment 5 to compartment 7")
    ("K67" "Rate constant from compartment 6 to compartment 7")
    ("K87" "Rate constant from compartment 8 to compartment 7")
    ("K97" "Rate constant from compartment 9 to compartment 7")
    ("K18" "Rate constant from compartment 1 to compartment 8")
    ("K28" "Rate constant from compartment 2 to compartment 8")
    ("K38" "Rate constant from compartment 3 to compartment 8")
    ("K48" "Rate constant from compartment 4 to compartment 8")
    ("K58" "Rate constant from compartment 5 to compartment 8")
    ("K68" "Rate constant from compartment 6 to compartment 8")
    ("K78" "Rate constant from compartment 7 to compartment 8")
    ("K98" "Rate constant from compartment 9 to compartment 8")
    ("K19" "Rate constant from compartment 1 to compartment 9")
    ("K29" "Rate constant from compartment 2 to compartment 9")
    ("K39" "Rate constant from compartment 3 to compartment 9")
    ("K49" "Rate constant from compartment 4 to compartment 9")
    ("K59" "Rate constant from compartment 5 to compartment 9")
    ("K69" "Rate constant from compartment 6 to compartment 9")
    ("K79" "Rate constant from compartment 7 to compartment 9")
    ("K89" "Rate constant from compartment 8 to compartment 9")
    ("K10" "Elimination Rate Constant from Compartment 1")
    ("K20" "Elimination Rate Constant from Compartment 2")
    ("K30" "Elimination Rate Constant from Compartment 3")
    ("K40" "Elimination Rate Constant from Compartment 4")
    ("K50" "Elimination Rate Constant from Compartment 5")
    ("K60" "Elimination Rate Constant from Compartment 6")
    ("K70" "Elimination Rate Constant from Compartment 7")
    ("K80" "Elimination Rate Constant from Compartment 8")
    ("K90" "Elimination Rate Constant from Compartment 9")
    ("TVK21" "Rate constant from compartment 2 to compartment 1")
    ("TVK31" "Rate constant from compartment 3 to compartment 1")
    ("TVK41" "Rate constant from compartment 4 to compartment 1")
    ("TVK51" "Rate constant from compartment 5 to compartment 1")
    ("TVK61" "Rate constant from compartment 6 to compartment 1")
    ("TVK71" "Rate constant from compartment 7 to compartment 1")
    ("TVK81" "Rate constant from compartment 8 to compartment 1")
    ("TVK91" "Rate constant from compartment 9 to compartment 1")
    ("TVK12" "Rate constant from compartment 1 to compartment 2")
    ("TVK32" "Rate constant from compartment 3 to compartment 2")
    ("TVK42" "Rate constant from compartment 4 to compartment 2")
    ("TVK52" "Rate constant from compartment 5 to compartment 2")
    ("TVK62" "Rate constant from compartment 6 to compartment 2")
    ("TVK72" "Rate constant from compartment 7 to compartment 2")
    ("TVK82" "Rate constant from compartment 8 to compartment 2")
    ("TVK92" "Rate constant from compartment 9 to compartment 2")
    ("TVK13" "Rate constant from compartment 1 to compartment 3")
    ("TVK23" "Rate constant from compartment 2 to compartment 3")
    ("TVK43" "Rate constant from compartment 4 to compartment 3")
    ("TVK53" "Rate constant from compartment 5 to compartment 3")
    ("TVK63" "Rate constant from compartment 6 to compartment 3")
    ("TVK73" "Rate constant from compartment 7 to compartment 3")
    ("TVK83" "Rate constant from compartment 8 to compartment 3")
    ("TVK93" "Rate constant from compartment 9 to compartment 3")
    ("TVK14" "Rate constant from compartment 1 to compartment 4")
    ("TVK24" "Rate constant from compartment 2 to compartment 4")
    ("TVK34" "Rate constant from compartment 3 to compartment 4")
    ("TVK54" "Rate constant from compartment 5 to compartment 4")
    ("TVK64" "Rate constant from compartment 6 to compartment 4")
    ("TVK74" "Rate constant from compartment 7 to compartment 4")
    ("TVK84" "Rate constant from compartment 8 to compartment 4")
    ("TVK94" "Rate constant from compartment 9 to compartment 4")
    ("TVK15" "Rate constant from compartment 1 to compartment 5")
    ("TVK25" "Rate constant from compartment 2 to compartment 5")
    ("TVK35" "Rate constant from compartment 3 to compartment 5")
    ("TVK45" "Rate constant from compartment 4 to compartment 5")
    ("TVK65" "Rate constant from compartment 6 to compartment 5")
    ("TVK75" "Rate constant from compartment 7 to compartment 5")
    ("TVK85" "Rate constant from compartment 8 to compartment 5")
    ("TVK95" "Rate constant from compartment 9 to compartment 5")
    ("TVK16" "Rate constant from compartment 1 to compartment 6")
    ("TVK26" "Rate constant from compartment 2 to compartment 6")
    ("TVK36" "Rate constant from compartment 3 to compartment 6")
    ("TVK46" "Rate constant from compartment 4 to compartment 6")
    ("TVK56" "Rate constant from compartment 5 to compartment 6")
    ("TVK76" "Rate constant from compartment 7 to compartment 6")
    ("TVK86" "Rate constant from compartment 8 to compartment 6")
    ("TVK96" "Rate constant from compartment 9 to compartment 6")
    ("TVK17" "Rate constant from compartment 1 to compartment 7")
    ("TVK27" "Rate constant from compartment 2 to compartment 7")
    ("TVK37" "Rate constant from compartment 3 to compartment 7")
    ("TVK47" "Rate constant from compartment 4 to compartment 7")
    ("TVK57" "Rate constant from compartment 5 to compartment 7")
    ("TVK67" "Rate constant from compartment 6 to compartment 7")
    ("TVK87" "Rate constant from compartment 8 to compartment 7")
    ("TVK97" "Rate constant from compartment 9 to compartment 7")
    ("TVK18" "Rate constant from compartment 1 to compartment 8")
    ("TVK28" "Rate constant from compartment 2 to compartment 8")
    ("TVK38" "Rate constant from compartment 3 to compartment 8")
    ("TVK48" "Rate constant from compartment 4 to compartment 8")
    ("TVK58" "Rate constant from compartment 5 to compartment 8")
    ("TVK68" "Rate constant from compartment 6 to compartment 8")
    ("TVK78" "Rate constant from compartment 7 to compartment 8")
    ("TVK98" "Rate constant from compartment 9 to compartment 8")
    ("TVK19" "Rate constant from compartment 1 to compartment 9")
    ("TVK29" "Rate constant from compartment 2 to compartment 9")
    ("TVK39" "Rate constant from compartment 3 to compartment 9")
    ("TVK49" "Rate constant from compartment 4 to compartment 9")
    ("TVK59" "Rate constant from compartment 5 to compartment 9")
    ("TVK69" "Rate constant from compartment 6 to compartment 9")
    ("TVK79" "Rate constant from compartment 7 to compartment 9")
    ("TVK89" "Rate constant from compartment 8 to compartment 9")
    ("TVK10" "Elimination Rate Constant from Compartment 1")
    ("TVK20" "Elimination Rate Constant from Compartment 2")
    ("TVK30" "Elimination Rate Constant from Compartment 3")
    ("TVK40" "Elimination Rate Constant from Compartment 4")
    ("TVK50" "Elimination Rate Constant from Compartment 5")
    ("TVK60" "Elimination Rate Constant from Compartment 6")
    ("TVK70" "Elimination Rate Constant from Compartment 7")
    ("TVK80" "Elimination Rate Constant from Compartment 8")
    ("TVK90" "Elimination Rate Constant from Compartment 9")
    ("S1" "Scale for 1st compartment")
    ("S2" "Scale for 2nd compartment")
    ("S3" "Scale for 3rd compartment")
    ("S4" "Scale for 4th compartment")
    ("S5" "Scale for 5th compartment")
    ("S6" "Scale for 6th compartment")
    ("S7" "Scale for 7th compartment")
    ("S8" "Scale for 8th compartment")
    ("S9" "Scale for 9th compartment")
    ("S0" "Alternate name for scale for output compartment")
    ("F1" "Bioavailability for 1st compartment")
    ("R1" "Rate for 1st compartment")
    ("D1" "Duration for 1st compartment")
    ("ALAG1" "Absorption lag for 1st compartment")
    ("F2" "Bioavailability for 2nd compartment")
    ("R2" "Rate for 2nd compartment")
    ("D2" "Duration for 2nd compartment")
    ("ALAG2" "Absorption lag for 2nd compartment")
    ("F3" "Bioavailability for 3rd compartment")
    ("R3" "Rate for 3rd compartment")
    ("D3" "Duration for 3rd compartment")
    ("ALAG3" "Absorption lag for 3rd compartment")
    ("F4" "Bioavailability for 4th compartment")
    ("R4" "Rate for 4th compartment")
    ("D4" "Duration for 4th compartment")
    ("ALAG4" "Absorption lag for 4th compartment")
    ("F5" "Bioavailability for 5th compartment")
    ("R5" "Rate for 5th compartment")
    ("D5" "Duration for 5th compartment")
    ("ALAG5" "Absorption lag for 5th compartment")
    ("F6" "Bioavailability for 6th compartment")
    ("R6" "Rate for 6th compartment")
    ("D6" "Duration for 6th compartment")
    ("ALAG6" "Absorption lag for 6th compartment")
    ("F7" "Bioavailability for 7th compartment")
    ("R7" "Rate for 7th compartment")
    ("D7" "Duration for 7th compartment")
    ("ALAG7" "Absorption lag for 7th compartment")
    ("F8" "Bioavailability for 8th compartment")
    ("R8" "Rate for 8th compartment")
    ("D8" "Duration for 8th compartment")
    ("ALAG8" "Absorption lag for 8th compartment")
    ("F9" "Bioavailability for 9th compartment")
    ("R9" "Rate for 9th compartment")
    ("D9" "Duration for 9th compartment")
    ("ALAG9" "Absorption lag for 9th compartment")
    ("F0" "Output fraction (also called Fm, FO) ")      
    ("XSCALE" "X parameter ")
    )
  "* Advan 5 variables")

(defvar esn-advan-trans-6
  '(
    ("S1" "Scale for 1st compartment ")
    ("F1" "Bioavailability for 1st compartment ")
    ("R1" "Rate for 1st compartment ")
    ("D1" "Duration for 1st compartment ")
    ("ALAG1" "Absorption lag for 1st compartment ")
    ("S2" "Scale for 2nd compartment ")
    ("F2" "Bioavailability for 2nd compartment ")
    ("R2" "Rate for 2nd compartment ")
    ("D2" "Duration for 2nd compartment ")
    ("ALAG2" "Absorption lag for 2nd compartment ")
    ("S3" "Scale for 3rd compartment ")
    ("F3" "Bioavailability for 3rd compartment ")
    ("R3" "Rate for 3rd compartment ")
    ("D3" "Duration for 3rd compartment ")
    ("ALAG3" "Absorption lag for 3rd compartment ")
    ("S4" "Scale for 4th compartment ")
    ("F4" "Bioavailability for 4th compartment ")
    ("R4" "Rate for 4th compartment ")
    ("D4" "Duration for 4th compartment ")
    ("ALAG4" "Absorption lag for 4th compartment ")
    ("S5" "Scale for 5th compartment ")
    ("F5" "Bioavailability for 5th compartment ")
    ("R5" "Rate for 5th compartment ")
    ("D5" "Duration for 5th compartment ")
    ("ALAG5" "Absorption lag for 5th compartment ")
    ("S6" "Scale for 6th compartment ")
    ("F6" "Bioavailability for 6th compartment ")
    ("R6" "Rate for 6th compartment ")
    ("D6" "Duration for 6th compartment ")
    ("ALAG6" "Absorption lag for 6th compartment ")
    ("S7" "Scale for 7th compartment ")
    ("F7" "Bioavailability for 7th compartment ")
    ("R7" "Rate for 7th compartment ")
    ("D7" "Duration for 7th compartment ")
    ("ALAG7" "Absorption lag for 7th compartment ")
    ("S8" "Scale for 8th compartment ")
    ("F8" "Bioavailability for 8th compartment ")
    ("R8" "Rate for 8th compartment ")
    ("D8" "Duration for 8th compartment ")
    ("ALAG8" "Absorption lag for 8th compartment ")
    ("S9" "Scale for 9th compartment ")
    ("F9" "Bioavailability for 9th compartment ")
    ("R9" "Rate for 9th compartment ")
    ("D9" "Duration for 9th compartment ")
    ("ALAG9" "Absorption lag for 9th compartment ")
    ("S0" "Alternate name for scale for output compartment ")
    ("F0" "Output fraction (also called Fm, FO) ")
    ("XSCALE" "X parameter "))
  "* Esn Advan 6 variables")

;;;###autoload
(defun esn-mode-short-advan (number)
  "Gets the short description of the advan number.

NUMBER specifies the advan number described."
  (let ((advan (assoc number esn-advan-trans-vars)))
    (if (not advan)
        ""
      (nth 1 advan))))

(defcustom esn-advan-trans-vars
  (append
   '(
     (1
      "One Compartment Linear Model"
      ";; -------------------------------------------------------------- ;;
;; One Compartment Linear Model (ADVAN1)                          ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; +---+---------+-----+-----+-----+-----+-----+          |       ;;
;; | # | CMPTNME | INI | AOn | ADs | DDs | DOb |        +-+-+     ;;
;; +---+---------+-----+-----+-----+-----+-----+        | 1 |     ;;
;; | 1 | Central | On. | No. | Yes | Yes | Yes |        +-+-+     ;;
;; +---+---------+-----+-----+-----+-----+-----+          |       ;;
;; | 2 | Output. | Off | Yes | No. | No. | No. |          V       ;;
;; +---+---------+-----+-----+-----+-----+-----+          2       ;;
;;                                                                ;;
;; #         = Compartment Number                                 ;;
;; CMPTNAME  = Compartment Name                                   ;;
;; INI       = Initial Status                                     ;;
;; AOn       = Allowed On/Off                                     ;;
;; ADs       = Dose Allowed                                       ;;
;; DDs       = Default for Dose                                   ;;
;; DOb       = Default for Observation                            ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; Shared PK parameters:                                          ;;
;;                                                                ;;
;;   S1         - Scale for central compartment (also called SC)  ;;
;;   S2         - Scale for output compartment (also called S0)   ;;
;;   F1         - Bioavailability for central compartment         ;;
;;   R1         - Rate for central compartment                    ;;
;;   D1         - Duration for central compartment                ;;
;;   ALAG1      - Absorption lag for central compartment          ;;
;;   F0         - Output fraction (also called F2, FO)            ;;
;;   XSCALE     - X parameter                                     ;;
;;                                                                ;;"
      (
       ("S1" "Scale for central compartment")
       ("S2" "Scale for output compartment (also called S0)")
       ("SC" "Scale for central compartment (also called S1)")
       ("S0" "Scale for output compartment (also called S2)")
       ("F1" "Bioavailability for central compartment")
       ("R1" "Rate for central compartment")
       ("D1" "Duration for central compartment")
       ("ALAG1" "Absorption lag for central compartment")
       ("F0" "Output fraction (also called F2, FO)")
       ("F2" "Output fraction (also called F2, FO)")
       ("XSCALE" "X parameter")
       )
      ( (1
         "In terms of K"
         ";; -------------------------------------------------------------- ;;
;; TRANS1                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; K = Rate of elimination (CL/V)                                 ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
         (
          ("K" "Individaul Rate of elimination (CL/V)")
          ("TVK" "Rate of elimination (CL/V)")
          
          ("V" "Individual volume of Distribution")
          ("TVV" "Volume of Distribution")
          )
         )
        (2
         "In terms of CL, and V"
         ";; -------------------------------------------------------------- ;;
;; TRANS2                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; CL           = Clearance                                       ;;
;; V            = Volume of Distribution                          ;;
;; Note: (CL/V) = K                                               ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
         (
          ("CL" "Indivdiaul Clearance")
          ("V" "Individual volume of Distribution")
          ("TVCL" "Clearance")
          ("TVV" "Volume of Distribution")
          )
         )
        )
      )
     (2 "One Compartment Linear Model with First Order Absorption"
        ";; -------------------------------------------------------------- ;;
;; One Compartment Linear Model with First Order Absorption       ;;
;; (ADVAN2)                                                       ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; +---+---------+-----+-----+-----+-----+-----+                  ;;
;; | # | CMPTNME | INI | AOn | ADs | DDs | DOb |         1        ;;
;; +---+---------+-----+-----+-----+-----+-----+         |        ;;
;; | 1 | DrgDepo | Off | Yes | Yes | Yes | No. |       +-+-+      ;;
;; +---+---------+-----+-----+-----+-----+-----+       | 2 |      ;;
;; | 2 | Central | On. | No. | Yes | No. | Yes |       +-+-+      ;;
;; +---+---------+-----+-----+-----+-----+-----+         |        ;;
;; | 3 | Output. | Off | Yes | No. | No. | No. |         V        ;;
;; +---+---------+-----+-----+-----+-----+-----+         3        ;;
;;                                                                ;;
;; #         = Compartment Number                                 ;;
;; CMPTNAME  = Compartment Name                                   ;;
;; INI       = Initial Status                                     ;;
;; AOn       = Allowed On/Off                                     ;;
;; ADs       = Dose Allowed                                       ;;
;; DDs       = Default for Dose                                   ;;
;; DOb       = Default for Observation                            ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; Shared PK parameters:                                          ;;
;;                                                                ;;
;;   S1         - Scale for depot compartment                     ;;
;;   S2         - Scale for central compartment (also called SC)  ;;
;;   S3         - Scale for output compartment (also called S0)   ;;
;;   F1         - Bioavailability for depot compartment           ;;
;;   F2         - Bioavailability for central compartment         ;;
;;   R1         - Rate for depot compartment                      ;;
;;   R2         - Rate for central compartment                    ;;
;;   D1         - Duration for depot compartment                  ;;
;;   D2         - Duration for central compartment                ;;
;;   ALAG1      - Absorption lag for depot compartment            ;;
;;   ALAG2      - Absorption lag for central compartment          ;;
;;   F0         - Output fraction (also called F3, FO)            ;;
;;   XSCALE     - X parameter                                     ;;"
        (
         ("S1" "Scale for depot compartment")
         ("S2" "Scale for central compartment (also called SC)")
         ("S3" "Scale for output compartment (also called S0)")
         ("SC" "Scale for central compartment (also called S2)")
         ("S0" "Scale for output compartment (also called S3)")
         ("F1" "Bioavailability for depot compartment")
         ("F2" "Bioavailability for central compartment")
         ("R1" "Rate for depot compartment")
         ("R2" "Rate for central compartment")
         ("D1" "Duration for depot compartment")
         ("D2" "Duration for central compartment")
         ("ALAG1" "Absorption lag for depot compartment")
         ("ALAG2" "Absorption lag for central compartment")
         ("F0" "Output fraction (also called F3, FO)")
         ("XSCALE" "X parameter")
         )
        (
         (1 "In terms of Ka, K"
            ";; -------------------------------------------------------------- ;;
;; TRANS1                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; K       = Rate of elimination (CL/V)                           ;;
;; Ka      = Absorption rate constant                             ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
            (
             ("K" "Rate of elimination (CL/V)")
             ("KA" "Absorption rate constant")
             ("TVK" "Rate of elimination (CL/V)")
             ("TVKA" "Absorption rate constant")
             ("TVV" "Volume of Distribution")
             ("V" "volume of Distribution")
             )
            )
         (2 "In terms of Ka, Cl, and V"
            ";; -------------------------------------------------------------- ;;
;; TRANS2                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; Ka           = Absorption rate constant                        ;;
;; CL           = Clearance                                       ;;
;; V            = Volume of Distribution                          ;;
;; Note: (CL/V) = K                                               ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
            (
             ("KA" "Absorption rate constant")
             ("CL" "Clearance")
             ("V" "volume of Distribution")
             ("TVKA" "Absorption rate constant")
             ("TVCL" "Clearance")
             ("TVV" "Volume of Distribution")
             )
            )
         )
        )
     (3 "Two Compartment Linear Mammillary Model"
        ";; -------------------------------------------------------------- ;;
;; Two Compartment Linear Mammillary Model (ADVAN3)               ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; +---+---------+-----+-----+-----+-----+-----+                  ;;
;; | # | CMPTNME | INI | AOn | ADs | DDs | DOb |                  ;;
;; +---+---------+-----+-----+-----+-----+-----+    |             ;;
;; | 1 | Central | On. | No. | Yes | Yes | Yes |  +-+-+   +-+-+   ;;
;; +---+---------+-----+-----+-----+-----+-----+  | 1 |<=>| 2 |   ;;
;; | 2 | Periph. | On. | No. | Yes | No. | No. |  +-+-+   +-+-+   ;;
;; +---+---------+-----+-----+-----+-----+-----+    |             ;;
;; | 3 | Output. | Off | Yes | No. | No. | No. |    V             ;;
;; +---+---------+-----+-----+-----+-----+-----+    3             ;;
;;                                                                ;;
;;                                                                ;;
;; #         = Compartment Number                                 ;;
;; CMPTNAME  = Compartment Name                                   ;;
;; INI       = Initial Status                                     ;;
;; AOn       = Allowed On/Off                                     ;;
;; ADs       = Dose Allowed                                       ;;
;; DDs       = Default for Dose                                   ;;
;; DOb       = Default for Observation                            ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; Shared PK parameters:                                          ;;
;;                                                                ;;
;;   S1         - Scale for central compartment (also called SC)  ;;
;;   S2         - Scale for peripheral compartment                ;;
;;   S3         - Scale for output compartment (also called S0)   ;;
;;   F1         - Bioavailability for central compartment         ;;
;;   F2         - Bioavailability for peripheral compartment      ;;
;;   R1         - Rate for central compartment                    ;;
;;   R2         - Rate for peripheral compartment                 ;;
;;   D1         - Duration for central compartment                ;;
;;   D2         - Duration for peripheral compartment             ;;
;;   ALAG1      - Absorption lag for central compartment          ;;
;;   ALAG2      - Absorption lag for peripheral compartment       ;;
;;   F0         - Output fraction (also called F3, FO)            ;;
;;   XSCALE     - X parameter                                     ;;
;;                                                                ;;"
        (
         ("S1" "Scale for central compartment (also called SC)")
         ("SC" "Scale for central compartment (also called S1)")
         ("S2" "Scale for peripheral compartment")
         ("S3" "Scale for output compartment (also called S0)")
         ("S0" "Scale for output compartment (also called S3)")
         ("F1" "Bioavailability for central compartment")
         ("F2" "Bioavailability for peripheral compartment")
         ("R1" "Rate for central compartment")
         ("R2" "Rate for peripheral compartment")
         ("D1" "Duration for central compartment")
         ("D2" "Duration for peripheral compartment")
         ("ALAG1" "Absorption lag for central compartment")
         ("ALAG2" "Absorption lag for peripheral compartment")
         ("F0" "Output fraction (also called F3, FO)")
         ("XSCALE" "X parameter")
         )
        (
         ( 1 "In terms of K, K12, and K21"
             ";; -------------------------------------------------------------- ;;
;; TRANS1                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; K    = Rate constant of elimination                            ;;
;; K12  = Rate constant from central to peripheral                ;;
;; K21  = Rate constant from perperial to central                 ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
             (
              ("K" "Rate constant of elimination")
              ("K12" "Rate constant from central to peripheral")
              ("K21" "Rate constant from perperial to central")
              ("TVK" "Rate constant of elimination")
              ("TVK12" "Rate constant from central to peripheral")
              ("TVK21" "Rate constant from perperial to central")
              ("TVV" "Central volume")
              ("V" "Central volume")
              )
             )
         ( 3 "In terms of CL, V, Q, and VSS"
             ";; -------------------------------------------------------------- ;;
;; TRANS3                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; CL   = Clearance                                               ;;
;; V    = Central Volume                                          ;;
;; Q    = Inter-compartmental clearance                           ;;
;; VSS  = Volume of distribution at steady-state                  ;;
;;                                                                ;;
;; Relationships:                                                 ;;
;;                                                                ;;
;; K    = CL/V                                                    ;;
;; K12  = Q/V                                                     ;;
;; K21  = Q/(Vss-V)                                               ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
             (
              ("CL" "Clearance")
              ("V" "Central volume")
              ("Q" "Inter-compartmental clearance")
              ("VSS" "volume of distribution at steady-state")
              ("TVCL" "Clearance")
              ("TVV" "Central volume")
              ("TVQ" "Inter-compartmental clearance")
              ("TVVSS" "Volume of distribution at steady-state")
              )
             )
         ( 4 "In Terms of CL, V1, Q, and V2"
             ";; -------------------------------------------------------------- ;;
;; TRANS4                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; CL   = Clearance                                               ;;
;; V1   = Central Volume                                          ;;
;; Q    = Inter-compartmental clearance                           ;;
;; V2   = Peripheral Volume                                       ;;
;;                                                                ;;
;; Relationships:                                                 ;;
;;                                                                ;;
;; K    = CL/V1                                                   ;;
;; K12  = Q/V1                                                    ;;
;; K21  = Q/V2                                                    ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
             (
              ("CL" "Clearance")
              ("V1" "Central volume")
              ("Q" "Inter-compartmental clearance")
              ("V2" "Peripheral volume")
              ("TVCL" "Clearance")
              ("TVV1" "Central volume")
              ("TVQ" "Inter-compartmental clearance")
              ("TVV2" "Peripheral volume")))
         ( 5 "In terms of AOB, ALPHA and BETA"
             ";; -------------------------------------------------------------- ;;
;; TRANS5                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; AOB = A/B                                                      ;;
;; ALPHA = Alpha elimination parameter                            ;;
;; BETA = Terminal elimination parameter                          ;;
;;                                                                ;;
;; Relationship:                                                  ;;
;;                                                                ;;
;; K21  = (AOB*BETA+ALPHA)/(AOB+1)                                ;;
;; K    = (ALPHA*BETA)/K21                                        ;;
;; K12  = ALPHA+BETA-K21-K                                        ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
             (
              ("AOB" "A/B")
              ("ALPHA" "Alpha elimination parameter")
              ("BETA" "Terminal elimination parameter")
              ("TVAOB" "A/B")
              ("TVALPHA" "Alpha elimination parameter")
              ("TVBETA" "Terminal elimination parameter")
              ("TVV" "Central volume")
              ("V" "Central volume")))
         ( 6 "In terms of ALPHA, BETA and K21"
             ";; -------------------------------------------------------------- ;;
;; TRANS6                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;;   ALPHA      = Alpha                                           ;;
;;   BETA       = Beta                                            ;;
;;   K21        = Rate constant (peripheral to central)           ;;
;;                                                                ;;
;; Relationship:                                                  ;;
;;                                                                ;;
;;   K          =ALPHA*BETA/K21                                   ;;
;;   K12        =ALPHA+BETA-K21-K                                 ;;
;;                                                                ;;
;; Constraint:                                                    ;;
;;                                                                ;;
;;   Assuming that ALPHA < BETA, then ALPHA < K21 < BETA.         ;;
;;   The roles of ALPHA and BETA are exchangeable.                ;;
;; -------------------------------------------------------------- ;;"
             (
              ("ALPHA" "Alpha")
              ("BETA" "Beta")
              ("K21" "Rate constant (peripheral to central)")
              ("TVALPHA" "Alpha")
              ("TVBETA" "Beta")
              ("TVK21" "Rate constant (peripheral to central)")
              ("TVV" "Central volume")
              ("V" "Central volume")))))
     ( 4 "Two Compartment Linear Mammillary Model w/First Order Absorpt."
         ";; -------------------------------------------------------------- ;;
;; Two Compartment Linear Mammillary Model (ADVAN4)               ;;
;; with First Order Absorption                                    ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; +---+---------+-----+-----+-----+-----+-----+                  ;;
;; | # | CMPTNME | INI | AOn | ADs | DDs | DOb |    1             ;;
;; +---+---------+-----+-----+-----+-----+-----+    |             ;;
;; | 1 | DrgDepo | Off | Yes | Yes | Yes | No. |  +-+-+   +-+-+   ;;
;; +---+---------+-----+-----+-----+-----+-----+  | 2 |<=>| 3 |   ;;
;; | 2 | Central | On. | No. | Yes | No. | Yes |  +-+-+   +-+-+   ;;
;; +---+---------+-----+-----+-----+-----+-----+    |             ;;
;; | 3 | Periph. | On. | No. | Yes | No. | No. |    V             ;;
;; +---+---------+-----+-----+-----+-----+-----+    4             ;;
;; | 4 | Output. | Off | Yes | No. | No. | No. |                  ;;
;; +---+---------+-----+-----+-----+-----+-----+                  ;;
;;                                                                ;;
;; #         = Compartment Number                                 ;;
;; CMPTNAME  = Compartment Name                                   ;;
;; INI       = Initial Status                                     ;;
;; AOn       = Allowed On/Off                                     ;;
;; ADs       = Dose Allowed                                       ;;
;; DDs       = Default for Dose                                   ;;
;; DOb       = Default for Observation                            ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; Shared PK parameters:                                          ;;
;;                                                                ;;
;;   S1         - Scale for depot compartment                     ;;
;;   S2         - Scale for central compartment (also called SC)  ;;
;;   S3         - Scale for peripheral compartment                ;;
;;   S4         - Scale for output compartment (also called S0)   ;;
;;   F1         - Bioavailability for depot compartment           ;;
;;   F2         - Bioavailability for central compartment         ;;
;;   F3         - Bioavailability for peripheral compartment      ;;
;;   R1         - Rate for depot compartment                      ;;
;;   R2         - Rate for central compartment                    ;;
;;   R3         - Rate for peripheral compartment                 ;;
;;   D1         - Duration for depot compartment                  ;;
;;   D2         - Duration for central compartment                ;;
;;   D3         - Duration for peripheral compartment             ;;
;;   ALAG1      - Absorption lag for depot compartment            ;;
;;   ALAG2      - Absorption lag for central compartment          ;;
;;   ALAG3      - Absorption lag for peripheral compartment       ;;
;;   F0         - Output fraction (also called F4, FO)            ;;
;;   XSCALE     - X parameter                                     ;;
;;                                                                ;;"
         (
          ("S1" "Scale for depot compartment")
          ("S2" "Scale for central compartment (also called SC)")
          ("S3" "Scale for peripheral compartment")
          ("S4" "Scale for output compartment (also called S0)")
          ("F1" "Bioavailability for depot compartment")
          ("F2" "Bioavailability for central compartment")
          ("F3" "Bioavailability for peripheral compartment")
          ("R1" "Rate for depot compartment")
          ("R2" "Rate for central compartment")
          ("R3" "Rate for peripheral compartment")
          ("D1" "Duration for depot compartment")
          ("D2" "Duration for central compartment")
          ("D3" "Duration for peripheral compartment")
          ("ALAG1" "Absorption lag for depot compartment")
          ("ALAG2" "Absorption lag for central compartment")
          ("ALAG3" "Absorption lag for peripheral compartment")
          ("F0" "Output fraction (also called F4, FO)")
          ("XSCALE" "X parameter"))
         (
          (1 "In terms of K, K21, K32, and KA"
             ";; -------------------------------------------------------------- ;;
;; TRANS1                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; K    = Rate constant of elimination                            ;;
;; K23  = Rate constant from central to peripheral                ;;
;; K32  = Rate constant from peripheral to central                ;;
;; KA   = Absorption rate constant                                ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
             
             (
              ("K" "Rate constant of elimination")
              ("K23" "Rate constant from central to peripheral")
              ("K32" "Rate constant from peripheral to central")
              ("KA" "Absorption rate constant")
              ("TVK" "Rate constant of elimination")
              ("TVK23" "Rate constant from central to peripheral")
              ("TVK32" "Rate constant from peripheral to central")
              ("TVKA" "Absorption rate constant")
              ("TVV" "Central volume")
              ("V" "Central volume")))
          (3 "In terms of CL, V, Q, VSS, and KA"
             ";; -------------------------------------------------------------- ;;
;; TRANS3                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; CL   = Clearance                                               ;;
;; V    = Central Volume                                          ;;
;; Q    = Inter-compartmental clearance                           ;;
;; VSS  = Volume of distribution at steady-state                  ;;
;;                                                                ;;
;; Relationships:                                                 ;;
;;                                                                ;;
;; K    = CL/V                                                    ;;
;; K12  = Q/V                                                     ;;
;; K21  = Q/(Vss-V)                                               ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
             
             (
              ("CL" "Clearance")
              ("V" "Central volume")
              ("Q" "Intercompartmental clearance")
              ("VSS" "volume of distribution at steady state")
              ("KA" "Absorption rate constant")
              ("TVCL" "Clearance")
              ("TVV" "Central volume")
              ("TVQ" "Intercompartmental clearance")
              ("TVVSS" "Volume of distribution at steady state")
              ("TVKA" "Absorption rate constant")))
          (4 "In terms of CL, V2, Q, V3, and KA"
             ";; -------------------------------------------------------------- ;;
;; TRANS4                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; CL   = Clearance                                               ;;
;; V1   = Central Volume                                          ;;
;; Q    = Inter-compartmental clearance                           ;;
;; V2   = Peripheral Volume                                       ;;
;;                                                                ;;
;; Relationships:                                                 ;;
;;                                                                ;;
;; K    = CL/V1                                                   ;;
;; K12  = Q/V1                                                    ;;
;; K21  = Q/V2                                                    ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
             
             (
              ("CL" "Clearance")
              ("V2" "Central volume")
              ("Q" "Intercompartmental clearance")
              ("V3" "Peripheral volume")
              ("KA" "Absorption rate constant")
              ("TVCL" "Clearance")
              ("TVV2" "Central volume")
              ("TVQ" "Intercompartmental clearance")
              ("TVV3" "Peripheral volume")
              ("TVKA" "Absorption rate constant")))
          (5 "In terms of AOB, ALPHA, BETA, and KA"
             ";; -------------------------------------------------------------- ;;
;; TRANS5                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; AOB = A/B                                                      ;;
;; ALPHA = Alpha elimination parameter                            ;;
;; BETA = Terminal elimination parameter                          ;;
;;                                                                ;;
;; Relationship:                                                  ;;
;;                                                                ;;
;; K21  = (AOB*BETA+ALPHA)/(AOB+1)                                ;;
;; K    = (ALPHA*BETA)/K21                                        ;;
;; K12  = ALPHA+BETA-K21-K                                        ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
             (
              ("AOB" "A/B")
              ("ALPHA" "Alpha elimination rate")
              ("BETA" "Beta/terminal elimination rate")
              ("KA" "Absorption rate constant")
              ("TVAOB" "A/B")
              ("TVALPHA" "Alpha elimination rate")
              ("TVBETA" "Beta/terminal elimination rate")
              ("TVKA" "Absorption rate constant")
              ("TVV" "Central volume")
              ("V" "Central volume")))
          (6 "In terms of ALPHA, BETA, K32, and KA"
             ";; -------------------------------------------------------------- ;;
;; TRANS6                                                         ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;;   ALPHA      = Alpha                                           ;;
;;   BETA       = Beta                                            ;;
;;   K21        = Rate constant (peripheral to central)           ;;
;;                                                                ;;
;; Relationship:                                                  ;;
;;                                                                ;;
;;   K          =ALPHA*BETA/K21                                   ;;
;;   K12        =ALPHA+BETA-K21-K                                 ;;
;;                                                                ;;
;; Constraint:                                                    ;;
;;                                                                ;;
;;   Assuming that ALPHA < BETA, then ALPHA < K21 < BETA.         ;;
;;   The roles of ALPHA and BETA are exchangeable.                ;;
;; -------------------------------------------------------------- ;;"
             (
              ("ALPHA" "Alpha")
              ("BETA" "Beta")
              ("K32" "Rate constant (peripheral to central)")
              ("KA" "Absorption rate constant")
              ("TVALPHA" "Alpha")
              ("TVBETA" "Beta")
              ("TVK32" "Rate constant (peripheral to central)")
              ("TVKA" "Absorption rate constant")
              ("TVV" "Central volume")
              ("V" "Central volume"))))))
   (list (list 5 "General Linear PK Model"
               ";; ---------------------------------------------------------------- ;;
;; General Linear PK Model (ADVAN5)                                 ;;
;; ---------------------------------------------------------------- ;;
;; Drug is distributed with linear kinetics between compartments.   ;;
;;                                                                  ;;
;; Eigenvalues can be real or imaginary.                            ;;
;;                                                                  ;;
;; A $MODEL record is required to describe the compartments and     ;;
;; their attributes.  The $PK record (or, if a user-supplied PK     ;;
;; routine is used, the $MODEL record) describes how the            ;;
;; compartments are linked.                                         ;;
;;                                                                  ;;
;; Suppose there are m compartments in the system, including the    ;;
;; output compartment.                                              ;;
;; ---------------------------------------------------------------- ;;
;; Basic PK parameters:                                             ;;
;;                                                                  ;;
;;   Kij (rate constant from compartment i to compartment j)        ;;
;;   Ki0 (alternate name for Kim)                                   ;;
;;                                                                  ;;
;; Additional PK parameters:                                        ;;
;;                                                                  ;;
;; For each compartment n in the system (n=1, ..., m):              ;;
;;                                                                  ;;
;;   Sn - Scale for nth compartment                                 ;;
;;   S0 - Alternate name for scale for output compartment           ;;
;;                                                                  ;;
;; For each dosable compartment n in the system:                    ;;
;;                                                                  ;;
;;   Fn         - Bioavailability for nth compartment               ;;
;;   Rn         - Rate for nth compartment                          ;;
;;   Dn         - Duration for nth compartment                      ;;
;;   ALAGn      - Absorption lag for nth compartment                ;;
;;                                                                  ;;
;; Other additional PK parameters:                                  ;;
;;                                                                  ;;
;;   F0         - Output fraction (also called Fm, FO)              ;;
;;   XSCALE     - X parameter                                       ;;
;; ---------------------------------------------------------------- ;;"
               esn-advan-trans-5)
         (list 6 "General Nonlinear Model"
               ";; --------------------------------------------------------------- ;;
;; General Nonlinear Model (ADVAN6)                                ;;
;; --------------------------------------------------------------- ;;
;; Drug is distributed in a first order process.                   ;;
;;                                                                 ;;
;; System is solved with a regular differential equation           ;;
;;                                                                 ;;
;; A $MODEL record is required to describe the compartments and    ;;
;; their attributes.  The $DES record is required to describe the  ;;
;; differential equations.                                         ;;
;;                                                                 ;;
;; TOL is required to specify the number of digits that this       ;;
;; routine will tolerate.                                          ;;
;;                                                                 ;;
;; Explicit Basic PK parameters:                                   ;;
;;                                                                 ;;
;;   P(n) (nth basic PK parameter)                                 ;;
;;                                                                 ;;
;; Implicit basic PK parameters:                                   ;;
;;                                                                 ;;
;;   PK-defined variables used also in $DES block.                 ;;
;;                                                                 ;;
;; Additional PK parameters:                                       ;;
;;                                                                 ;;
;; Suppose there are m compartments in the system, including the   ;;
;; output compartment.  For each compartment n in the system (n=1, ;;
;; ..., m):                                                        ;;
;;                                                                 ;;
;;   Sn - Scale for nth compartment                                ;;
;;   S0 - Alternate name for scale for output compartment          ;;
;;                                                                 ;;
;; For each dosable compartment n in the system:                   ;;
;;                                                                 ;;
;;   Fn         - Bioavailability for nth compartment              ;;
;;   Rn         - Rate for nth compartment                         ;;
;;   Dn         - Duration for nth compartment                     ;;
;;   ALAGn      - Absorption lag for nth compartment               ;;
;;                                                                 ;;
;; Other additional PK parameters:                                 ;;
;;                                                                 ;;
;;   F0         - Output fraction (also called Fm, FO)             ;;
;;   XSCALE     - X parameter                                      ;;
;; --------------------------------------------------------------- ;;"
               esn-advan-trans-6)
         (list 7 "General Linear PK Model with Real Eigenvalues"
               ";; ---------------------------------------------------------------- ;;
;; General Linear PK Model with Real Eigenvalues (ADVAN7)           ;;
;; ---------------------------------------------------------------- ;;
;; Drug is distributed with linear kinetics between compartments.   ;;
;;                                                                  ;;
;; Eigenvalues are real.                                            ;;
;;                                                                  ;;
;; A $MODEL record is required to describe the compartments and     ;;
;; their attributes.  The $PK record (or, if a user-supplied PK     ;;
;; routine is used, the $MODEL record) describes how the            ;;
;; compartments are linked.                                         ;;
;;                                                                  ;;
;; Suppose there are m compartments in the system, including the    ;;
;; output compartment.                                              ;;
;; ---------------------------------------------------------------- ;;
;; Basic PK parameters:                                             ;;
;;                                                                  ;;
;;   Kij (rate constant from compartment i to compartment j)        ;;
;;   Ki0 (alternate name for Kim)                                   ;;
;;                                                                  ;;
;; Additional PK parameters:                                        ;;
;;                                                                  ;;
;; For each compartment n in the system (n=1, ..., m):              ;;
;;                                                                  ;;
;;   Sn - Scale for nth compartment                                 ;;
;;   S0 - Alternate name for scale for output compartment           ;;
;;                                                                  ;;
;; For each dosable compartment n in the system:                    ;;
;;                                                                  ;;
;;   Fn         - Bioavailability for nth compartment               ;;
;;   Rn         - Rate for nth compartment                          ;;
;;   Dn         - Duration for nth compartment                      ;;
;;   ALAGn      - Absorption lag for nth compartment                ;;
;;                                                                  ;;
;; Other additional PK parameters:                                  ;;
;;                                                                  ;;
;;   F0         - Output fraction (also called Fm, FO)              ;;
;;   XSCALE     - X parameter                                       ;;
;; ---------------------------------------------------------------- ;;"

               esn-advan-trans-5)
         (list 8 "General Nonlinear Model with Stiff Differential Equations"
               ";; --------------------------------------------------------------- ;;
;; General Nonlinear Model with Stiff Differential Equations       ;;
;; (ADVAN8)                                                        ;;
;; --------------------------------------------------------------- ;;
;; Drug is distributed in a first order process.                   ;;
;;                                                                 ;;
;; System is solved with a stiff differential equation             ;;
;;                                                                 ;;
;; A $MODEL record is required to describe the compartments and    ;;
;; their attributes.  The $DES record is required to describe the  ;;
;; differential equations.                                         ;;
;;                                                                 ;;
;; TOL is required to specify the number of digits that this       ;;
;; routine will tolerate.                                          ;;
;;                                                                 ;;
;; Explicit Basic PK parameters:                                   ;;
;;                                                                 ;;
;;   P(n) (nth basic PK parameter)                                 ;;
;;                                                                 ;;
;; Implicit basic PK parameters:                                   ;;
;;                                                                 ;;
;;   PK-defined variables used also in $DES block Additional PK    ;;
;;   parameters:                                                   ;;
;;                                                                 ;;
;; Suppose there are m compartments in the system, including the   ;;
;; output compartment.  For each compartment n in the system (n=1, ;;
;; ..., m):                                                        ;;
;;                                                                 ;;
;;   Sn - Scale for nth compartment                                ;;
;;   S0 - Alternate name for scale for output compartment          ;;
;;                                                                 ;;
;; For each dosable compartment n in the system:                   ;;
;;                                                                 ;;
;;   Fn         - Bioavailability for nth compartment              ;;
;;   Rn         - Rate for nth compartment                         ;;
;;   Dn         - Duration for nth compartment                     ;;
;;   ALAGn      - Absorption lag for nth compartment               ;;
;;                                                                 ;;
;; Other additional PK parameters:                                 ;;
;;                                                                 ;;
;;   F0         - Output fraction (also called Fm, FO)             ;;
;;   XSCALE     - X parameter                                      ;;
;; --------------------------------------------------------------- ;;"
               esn-advan-trans-6)
         (list 9 "General Nonlinear Model with Equilibrium Compartments"
               ";; --------------------------------------------------------------- ;;
;; General Nonlinear Model with Equilibrium Compartments (ADVAN9)  ;;
;; --------------------------------------------------------------- ;;
;; A $MODEL record is required to describe the compartments and    ;;
;; their attributes.  The $DES record describes the differential   ;;
;; equations, if any.  The $AES and $AESINITIAL records describe   ;;
;; the algebraic expressions, if any.                              ;;
;;                                                                 ;;
;; TOL is required to specify the number of digits that this       ;;
;; routine will tolerate.                                          ;;
;;                                                                 ;;
;; Explicit Basic PK parameters:                                   ;;
;;                                                                 ;;
;;  P(n) (nth basic PK parameter)                                  ;;
;;                                                                 ;;
;; Implicit basic PK parameters:                                   ;;
;;                                                                 ;;
;;   PK-defined variables used also in $AES or $DES blocks         ;;
;;                                                                 ;;
;; Additional PK parameters:                                       ;;
;;                                                                 ;;
;; Suppose there are m compartments in the system, including the   ;;
;; output compartment.  For each compartment n in the system (n=1, ;;
;; ..., m):                                                        ;;
;;                                                                 ;;
;;   Sn - Scale for nth compartment                                ;;
;;   S0 - Alternate name for scale for output compartment          ;;
;;                                                                 ;;
;; For each dosable compartment n in the system:                   ;;
;;                                                                 ;;
;;   Fn         - Bioavailability for nth compartment              ;;
;;   Rn         - Rate for nth compartment                         ;;
;;   Dn         - Duration for nth compartment                     ;;
;;   ALAGn      - Absorption lag for nth compartment               ;;
;;                                                                 ;;
;; Other additional PK parameters:                                 ;;
;;                                                                 ;;
;;   F0         - Output fraction (also called Fm, FO)             ;;
;;   XSCALE     - X parameter                                      ;;
;;                                                                 ;;
;; The user may find that the TOL option of the $SUBROUTINE record ;;
;; (or the $TOL record) should specify larger NRD values than for  ;;
;; other ADVANs (e.g., ADVAN6).  Values of 7 or 8 may not be       ;;
;; unreasonable with double precision.                             ;;
;;                                                                 ;;
;; If there is a period of time during which some compartment's    ;;
;; amount should be zero, that compartment should be turned off.   ;;
;; Otherwise, very small amounts can appear in the compartment,    ;;
;; which can cause difficulties during subsequent time periods.    ;;
;; --------------------------------------------------------------- ;;"
               esn-advan-trans-6)
         (list 13 "General Nonlinear Model (LSODA Solver)"

               ";; --------------------------------------------------------------- ;;
;; General Nonlinear Model (ADVAN13)                               ;;
;; --------------------------------------------------------------- ;;
;; Drug is distributed in a first order process.                   ;;
;;                                                                 ;;
;; System is solved with a differential equation solver LSODA      ;;
;;                                                                 ;;
;; A $MODEL record is required to describe the compartments and    ;;
;; their attributes.  The $DES record is required to describe the  ;;
;; differential equations.                                         ;;
;;                                                                 ;;
;; TOL is required to specify the number of digits that this       ;;
;; routine will tolerate.                                          ;;
;;                                                                 ;;
;; Explicit Basic PK parameters:                                   ;;
;;                                                                 ;;
;;   P(n) (nth basic PK parameter)                                 ;;
;;                                                                 ;;
;; Implicit basic PK parameters:                                   ;;
;;                                                                 ;;
;;   PK-defined variables used also in $DES block.                 ;;
;;                                                                 ;;
;; Additional PK parameters:                                       ;;
;;                                                                 ;;
;; Suppose there are m compartments in the system, including the   ;;
;; output compartment.  For each compartment n in the system (n=1, ;;
;; ..., m):                                                        ;;
;;                                                                 ;;
;;   Sn - Scale for nth compartment                                ;;
;;   S0 - Alternate name for scale for output compartment          ;;
;;                                                                 ;;
;; For each dosable compartment n in the system:                   ;;
;;                                                                 ;;
;;   Fn         - Bioavailability for nth compartment              ;;
;;   Rn         - Rate for nth compartment                         ;;
;;   Dn         - Duration for nth compartment                     ;;
;;   ALAGn      - Absorption lag for nth compartment               ;;
;;                                                                 ;;
;; Other additional PK parameters:                                 ;;
;;                                                                 ;;
;;   F0         - Output fraction (also called Fm, FO)             ;;
;;   XSCALE     - X parameter                                      ;;
;; --------------------------------------------------------------- ;;"

               esn-advan-trans-6))
   '(
     ( 10 "One Compartment Model with Michaelis-Menten Elimination"
          ";; -------------------------------------------------------------- ;;
;; One Compartment Model with Michaelis-Menten Elimination        ;;
;; (ADVAN10)                                                      ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; +---+---------+-----+-----+-----+-----+-----+          |       ;;
;; | # | CMPTNME | INI | AOn | ADs | DDs | DOb |        +-+-+     ;;
;; +---+---------+-----+-----+-----+-----+-----+        | 1 |     ;;
;; | 1 | Central | On. | No. | Yes | Yes | Yes |        +-+-+     ;;
;; +---+---------+-----+-----+-----+-----+-----+          |       ;;
;; | 2 | Output. | Off | Yes | No. | No. | No. |          V       ;;
;; +---+---------+-----+-----+-----+-----+-----+          2       ;;
;;                                                                ;;
;; #         = Compartment Number                                 ;;
;; CMPTNAME  = Compartment Name                                   ;;
;; INI       = Initial Status                                     ;;
;; AOn       = Allowed On/Off                                     ;;
;; ADs       = Dose Allowed                                       ;;
;; DDs       = Default for Dose                                   ;;
;; DOb       = Default for Observation                            ;;
;;                                                                ;;
;; This assumes Michaelis-Menton Elimination                      ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; Basic PK parameters                                            ;;
;;                                                                ;;
;;   VM - (maximum rate)                                          ;;
;;   KM - (Michaelis constant)                                    ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;
;;                                                                ;;
;; Additional PK parameters:                                      ;;
;;                                                                ;;
;;   S1         - Scale for central compartment (also called SC)  ;;
;;   S2         - Scale for output compartment (also called S0)   ;;
;;   F1         - Bioavailability for central compartment         ;;
;;   R1         - Rate for central compartment                    ;;
;;   D1         - Duration for central compartment                ;;
;;   ALAG1      - Absorption lag for central compartment          ;;
;;   F0         - Output fraction (also called F2, FO)            ;;
;;   XSCALE     - X parameter                                     ;;
;;                                                                ;;
;; -------------------------------------------------------------- ;;"
          (
           ("S1" "Scale for central compartment (also called SC)")
           ("S2" "Scale for output compartment (also called S0)")
           ("F1" "Bioavailability for central compartment")
           ("R1" "Rate for central compartment")
           ("D1" "Duration for central compartment")
           ("ALAG1" "Absorption lag for central compartment")
           ("F0" "Output fraction (also called F2, FO)")
           ("XSCALE" "X parameter"))
          ( (1 ""
               ""
               (
                ("VM" "Maximum rate")
                ("KM" "Michaelis constant")
                ("TVVM" "Typical maximum rate")
                ("TVKM" "Typical michaelis constant")
                ("TVV" "Central volume")
                ("V" "Central volume")))))
     ( 11 "Three Compartment Linear Mammillary Model"
          ";; ---------------------------------------------------------------- ;;
;; Three Compartment Linear Mammillary Model (ADVAN11)              ;;
;; ---------------------------------------------------------------- ;;
;;                                                                  ;;
;; +---+---------+-----+-----+-----+-----+-----+     +-+-+          ;;
;; | # | CMPTNME | INI | AOn | ADs | DDs | DOb |     | 2 |          ;;
;; +---+---------+-----+-----+-----+-----+-----+     +-+-+          ;;
;; | 1 | Central | On. | No. | Yes | Yes | Yes |      | |           ;;
;; +---+---------+-----+-----+-----+-----+-----+     +-+-+ +-+-+    ;;
;; | 2 | Periph1 | On. | No. | Yes | No. | No. |    -+ 1 |=| 3 |    ;;
;; +---+---------+-----+-----+-----+-----+-----+     +-+-+ +-+-+    ;;
;; | 3 | Periph2 | On. | No. | Yes | No. | No. |       |            ;;
;; +---+---------+-----+-----+-----+-----+-----+       V            ;;
;; | 4 | Output. | Off | Yes | No. | No. | No. |       4            ;;
;; +---+---------+-----+-----+-----+-----+-----+                    ;;
;;                                                                  ;;
;;                                                                  ;;
;; #         = Compartment Number                                   ;;
;; CMPTNAME  = Compartment Name                                     ;;
;; INI       = Initial Status                                       ;;
;; AOn       = Allowed On/Off                                       ;;
;; ADs       = Dose Allowed                                         ;;
;; DDs       = Default for Dose                                     ;;
;; DOb       = Default for Observation                              ;;
;;                                                                  ;;
;; ---------------------------------------------------------------- ;;
;;                                                                  ;;
;; Shared PK Parameters:                                            ;;
;;   S1         - Scale for central compartment (also called SC)    ;;
;;   S2         - Scale for peripheral compartment 1                ;;
;;   S3         - Scale for peripheral compartment 2                ;;
;;   S4         - Scale for output compartment (also called S0)     ;;
;;   F1         - Bioavailability for central compartment           ;;
;;   F2         - Bioavailability for peripheral compartment 1      ;;
;;   F3         - Bioavailability for peripheral compartment 2      ;;
;;   R1         - Rate for central compartment                      ;;
;;   R2         - Rate for peripheral compartment 1                 ;;
;;   R3         - Rate for peripheral compartment 2                 ;;
;;   D1         - Duration for central compartment                  ;;
;;   D2         - Duration for peripheral compartment 1             ;;
;;   D3         - Duration for peripheral compartment 2             ;;
;;   ALAG1      - Absorption lag for central compartment            ;;
;;   ALAG2      - Absorption lag for peripheral compartment 1       ;;
;;   ALAG3      - Absorption lag for peripheral compartment 2       ;;
;;   F0         - Output fraction (also called F4, FO)              ;;
;;   XSCALE     - X parameter                                       ;;
;;                                                                  ;;
;; ---------------------------------------------------------------- ;;
;; Notes:                                                           ;;
;;                                                                  ;;
;; Analytical second derivatives used with the Laplacian method are ;;
;; not obtained with ADVAN11.  Numerical second derivatives must be ;;
;; used.                                                            ;;
;;                                                                  ;;
;; Can be greatly speeded up if calls to PK can be limited.  (Use   ;;
;; CALLFL=1 in $PK to call once per IR, and use the CALL data item  ;;
;; to force additional calls if and when necessary; e.g., when      ;;
;; covariates used in the PK model change.)                         ;;"
          (
           ("S1" "Scale for central compartment (also called SC)")
           ("S2" "Scale for peripheral compartment 1")
           ("S3" "Scale for peripheral compartment 2")
           ("S4" "Scale for output compartment (also called S0)")
           ("F1" "Bioavailability for central compartment")
           ("F2" "Bioavailability for peripheral compartment 1")
           ("F3" "Bioavailability for peripheral compartment 2")
           ("R1" "Rate for central compartment")
           ("R2" "Rate for peripheral compartment 1")
           ("R3" "Rate for peripheral compartment 2")
           ("D1" "Duration for central compartment")
           ("D2" "Duration for peripheral compartment 1")
           ("D3" "Duration for peripheral compartment 2")
           ("ALAG1" "Absorption lag for central compartment")
           ("ALAG2" "Absorption lag for peripheral compartment 1")
           ("ALAG3" "Absorption lag for peripheral compartment 2")
           ("F0" "Output fraction (also called F4, FO)")
           ("XSCALE" "X parameter"))
          (
           ( 1 "In terms of K, K12, K21, K13, and K31"
               ";; --------------------------------------------------------------   ;;
;; TRANS1                                                           ;;
;; --------------------------------------------------------------   ;;
;;                                                                  ;;
;; K    = Rate constant of elimination                              ;;
;; K12  = Rate constant from central to peripheral 1                ;;
;; K21  = Rate constant from peripheral 1 to central                ;;
;; K13  = Rate constant from central to peripheral 2                ;;
;; K31  = Rate constant from peripheral 2 to central                ;;
;;                                                                  ;;
;; ---------------------------------------------------------------- ;;"
               (
                ("K" "Rate constant of elimination")
                ("K12" "Rate constant from central to peripheral 1")
                ("K21" "Rate constant from peripheral 1 to central")
                ("K13" "Rate constant from central to peripheral 2")
                ("K31" "Rate constant from peripheral 2 to central")
                ("TVK" "Typical rate constant of elimination")
                ("TVK12" "Typical rate constant from central to peripheral 1")
                ("TVK21" "Typical rate constant from peripheral 1 to central")
                ("TVK13" "Typical rate constant from central to peripheral 2")
                ("TVK31" "Typical rate constant from peripheral 2 to central")
                ("TVV" "Central volume")
                ("V" "Central volume")))
           ( 4 "In terms of CL, V1, Q2, V2, Q3, and V3"
               ";; ---------------------------------------------------------------- ;;
;; TRANS4                                                           ;;
;; ---------------------------------------------------------------- ;;
;;                                                                  ;;
;; CL = Clearance                                                   ;;
;; V1 = Central volume                                              ;;
;; Q2 = Intercompartmental clearance (central and periph 1)         ;;
;; V2 = Peripheral 1 volume                                         ;;
;; Q3 = Intercompartmental clearance (central and periph 2)         ;;
;; V3 = Peripheral 2 volume                                         ;;
;;                                                                  ;;
;; Relationship:                                                    ;;
;;                                                                  ;;
;; K    = CL/V1                                                     ;;
;; K12  = Q2/V1                                                     ;;
;; K21  = Q2/V2                                                     ;;
;; K13  = Q3/V1                                                     ;;
;; K31  = Q3/V3                                                     ;;
;; ---------------------------------------------------------------- ;;"
               (
                ("CL" "Clearance")
                ("V1" "Central volume")
                ("Q2" "Intercompartmental clearance (central and periph 1)")
                ("V2" "Peripheral 1 volume")
                ("Q3" "Intercompartmental clearance (central and periph 2)")
                ("V3" "Peripheral 2 volume")
                ("TVCL" "Typical clearance")
                ("TVV1" "Typical central volume")
                ("TVQ2" "Typical intercompartmental clearance (central and periph 1)")
                ("TVV2" "Typical peripheral 1 volume")
                ("TVQ3" "Typical intercompartmental clearance (central and periph 2)")
                ("TVV3" "Typical peripheral 2 volume")))
           ( 6 "In terms of ALPHA, BETA, GAMMA, K21, and K31"
               ";; ---------------------------------------------------------------- ;;
;; TRANS6                                                           ;;
;; ---------------------------------------------------------------- ;;
;;                                                                  ;;
;; ALPHA        = Alpha                                             ;;
;; BETA         = Beta                                              ;;
;; GAMMA        = Gamma                                             ;;
;; K21          = Rate constant (peripheral 1 to central)           ;;
;; K31          = Rate constant (peripheral 2 to central)           ;;
;;                                                                  ;;
;; Relationship:                                                    ;;
;;                                                                  ;;
;; K    = ALPHA*BETA*GAMMA/(K21*K31)                                ;;
;; K13  = (P+K31*K31-K31*S-K*K21)/(K21-K31)                         ;;
;; K12  = S-K-K13-K21-K31                                           ;;
;;                                                                  ;;
;; where                                                            ;;
;;                                                                  ;;
;; S    = ALPHA+BETA+GAMMA                                          ;;
;; P    = ALPHA*BETA+ALPHA*GAMMA+BETA*GAMMA                         ;;
;;                                                                  ;;
;; Constraint:                                                      ;;
;;                                                                  ;;
;; Assuming that ALPHA<BETA<GAMMA, then  ALPHA<K21<BETA<K31<GAMMA   ;;
;;                                                                  ;;
;; or                                                               ;;
;;                                                                  ;;
;; ALPHA<K31<BETA<K21<GAMMA.                                        ;;
;;                                                                  ;;
;; The roles of ALPHA, BETA, GAMMA, K32, K42, K23,  and  K24  are   ;;
;; symmetric and are exchangeable.                                  ;;
;; ---------------------------------------------------------------- ;;"
               (
                ("ALPHA" "Alpha")
                ("BETA" "Beta")
                ("GAMMA" "Gamma")
                ("K21" "Rate constant (peripheral 1 to central)")
                ("K31" "Rate constant (peripheral 2 to central)")
                ("TVALPHA" "Typical alpha")
                ("TVBETA" "Typical beta")
                ("TVGAMMA" "Typical gamma")
                ("TVK21" "Typical rate constant (peripheral 1 to central)")
                ("TVK31" "Typical rate constant (peripheral 2 to central)")
                ("TVV" "Central volume")
                ("V" "Central volume")))))
     ( 12 "Three Compartment Linear Model with First Order Absorption"
          ";; ---------------------------------------------------------------- ;;
;; Three Compartment Linear Model with First Order Absorption       ;;
;; (ADVAN12)                                                        ;;
;; ---------------------------------------------------------------- ;;
;;                                                                  ;;
;; +---+---------+-----+-----+-----+-----+-----+                    ;;
;; | # | CMPTNME | INI | AOn | ADs | DDs | DOb |      +-+-+         ;;
;; +---+---------+-----+-----+-----+-----+-----+      | 4 |         ;;
;; | 1 |  Depot  | Off | Yes | Yes | Yes | No. |      +-+-+         ;;
;; +---+---------+-----+-----+-----+-----+-----+       | |          ;;
;; | 2 | Central | On. | No. | Yes | No. | Yes |      +-+-+ +-+-+   ;;
;; +---+---------+-----+-----+-----+-----+-----+   1 -+ 2 |=| 3 |   ;;
;; | 3 | Periph1 | On. | No. | Yes | No. | No. |      +-+-+ +-+-+   ;;
;; +---+---------+-----+-----+-----+-----+-----+        |           ;;
;; | 4 | Periph2 | On. | No. | Yes | No. | No. |        V           ;;
;; +---+---------+-----+-----+-----+-----+-----+        5           ;;
;; | 5 | Output. | Off | Yes | No. | No. | No. |                    ;;
;; +---+---------+-----+-----+-----+-----+-----+                    ;;
;;                                                                  ;;
;; #         = Compartment Number                                   ;;
;; CMPTNAME  = Compartment Name                                     ;;
;; INI       = Initial Status                                       ;;
;; AOn       = Allowed On/Off                                       ;;
;; ADs       = Dose Allowed                                         ;;
;; DDs       = Default for Dose                                     ;;
;; DOb       = Default for Observation                              ;;
;;                                                                  ;;
;; ---------------------------------------------------------------- ;;
;;                                                                  ;;
;; Shared PK Parameters:                                            ;;
;;                                                                  ;;
;; S1           - Scale for depot compartment                       ;;
;; S2           - Scale for central compartment (also called SC)    ;;
;; S3           - Scale for peripheral compartment 1                ;;
;; S4           - Scale for peripheral compartment 2                ;;
;; S5           - Scale for output compartment (also called S0)     ;;
;; F1           - Bioavailability for depot compartment             ;;
;; F2           - Bioavailability for central compartment           ;;
;; F3           - Bioavailability for peripheral compartment 1      ;;
;; F4           - Bioavailability for peripheral compartment 2      ;;
;; R1           - Rate for depot compartment                        ;;
;; R2           - Rate for central compartment                      ;;
;; R3           - Rate for peripheral compartment 1                 ;;
;; R4           - Rate for peripheral compartment 2                 ;;
;; D1           - Duration for depot compartment                    ;;
;; D2           - Duration for central compartment                  ;;
;; D3           - Duration for peripheral compartment 1             ;;
;; D4           - Duration for peripheral compartment 2             ;;
;; ALAG1        - Absorption lag for depot compartment              ;;
;; ALAG2        - Absorption lag for central compartment            ;;
;; ALAG3        - Absorption lag for peripheral compartment 1       ;;
;; ALAG4        - Absorption lag for peripheral compartment 2       ;;
;; F0           - Output fraction (also called F5, FO)              ;;
;; XSCALE       - X parameter                                       ;;
;;                                                                  ;;
;; ---------------------------------------------------------------- ;;
;; Notes:                                                           ;;
;;                                                                  ;;
;; Analytical second derivatives used with the Laplacian method are ;;
;; not obtained with ADVAN12.  Numerical second derivatives must be ;;
;; used.                                                            ;;
;;                                                                  ;;
;; Can be greatly speeded up if calls to PK can be limited.  (Use   ;;
;; CALLFL=1 in $PK to call once per IR, and use the CALL data item  ;;
;; to force additional calls if and when necessary; e.g., when      ;;
;; covariates used in the PK model change.)                         ;;"
          (
           ("S1" "Scale for depot compartment")
           ("S2" "Scale for central compartment (also called SC)")
           ("S3" "Scale for peripheral compartment 1")
           ("S4" "Scale for peripheral compartment 2")
           ("S5" "Scale for output compartment (also called S0)")
           ("F1" "Bioavailability for depot compartment")
           ("F2" "Bioavailability for central compartment")
           ("F3" "Bioavailability for peripheral compartment 1")
           ("F4" "Bioavailability for peripheral compartment 2")
           ("R1" "Rate for depot compartment")
           ("R2" "Rate for central compartment")
           ("R3" "Rate for peripheral compartment 1")
           ("R4" "Rate for peripheral compartment 2")
           ("D1" "Duration for depot compartment")
           ("D2" "Duration for central compartment")
           ("D3" "Duration for peripheral compartment 1")
           ("D4" "Duration for peripheral compartment 2")
           ("ALAG1" "Absorption lag for depot compartment")
           ("ALAG2" "Absorption lag for central compartment")
           ("ALAG3" "Absorption lag for peripheral compartment 1")
           ("ALAG4" "Absorption lag for peripheral compartment 2")
           ("F0" "Output fraction (also called F5, FO)")
           ("XSCALE" "X parameter"))
          ( ( 1 "In terms of K, K23, K32, K24, K42, and KA"
                ";; ---------------------------------------------------------------- ;;
;; TRANS1                                                           ;;
;; ---------------------------------------------------------------- ;;
;; K    = Rate constant of elimination                              ;;
;; K23  = Rate constant from central to peripheral 1                ;;
;; K32  = Rate constant from peripheral 1 to central                ;;
;; K24  = Rate constant from central to peripheral 2                ;;
;; K42  = Rate constant from peripheral 2 to central                ;;
;; KA   = Rate constant of absorption                               ;;
;; ---------------------------------------------------------------- ;;"
                (
                 ("K" "Rate constant of elimination")
                 ("K23" "Rate constant from central to peripheral 1")
                 ("K32" "Rate constant from peripheral 1 to central")
                 ("K24" "Rate constant from central to peripheral 2")
                 ("K42" "Rate constant from peripheral 2 to central")
                 ("KA" "Rate constant of absorption")
                 ("TVK" "Typical rate constant of elimination")
                 ("TVK23" "Typical rate constant from central to peripheral 1")
                 ("TVK32" "Typical rate constant from peripheral 1 to central")
                 ("TVK24" "Typical rate constant from central to peripheral 2")
                 ("TVK42" "Typical rate constant from peripheral 2 to central")
                 ("TVKA" "Typical rate constant of absorption")
                 ("TVV" "Central volume")
                 ("V" "Central volume")))
            ( 4 "In terms of CL, V2, Q3, V3, Q4, V4, and KA "
                ";; ---------------------------------------------------------------- ;;
;; TRANS4                                                           ;;
;; ---------------------------------------------------------------- ;;
;;                                                                  ;;
;; CL = Clearance                                                   ;;
;; V2 = Central volume                                              ;;
;; Q3 = Intercompartmental clearance (central and periph 1)         ;;
;; V3 = Peripheral 1 volume                                         ;;
;; Q4 = Intercompartmental clearance (central and periph 2)         ;;
;; V4 = Peripheral 2 volume                                         ;;
;; KA = Absorption rate constant                                    ;;
;;                                                                  ;;
;; Relationship:                                                    ;;
;;                                                                  ;;
;; K    = CL/V2                                                     ;;
;; K23  = Q3/V2                                                     ;;
;; K32  = Q3/V3                                                     ;;
;; K24  = Q4/V2                                                     ;;
;; K42  = Q4/V4                                                     ;;
;; KA   = KA                                                        ;;
;; ---------------------------------------------------------------- ;;"
                (
                 ("CL" "Clearance")
                 ("V2" "Central volume")
                 ("Q3" "Intercompartmental clearance (central and periph 1)")
                 ("V3" "Peripheral 1 volume")
                 ("Q4" "Intercompartmental clearance (central and periph 2)")
                 ("V4" "Peripheral 2 volume")
                 ("KA" "Absorption rate constant")
                 ("TVCL" "Typical clearance")
                 ("TVV2" "Typical central volume")
                 ("TVQ3" "Typical intercompartmental clearance (central and periph 1)")
                 ("TVV3" "Typical peripheral 1 volume")
                 ("TVQ4" "Typical intercompartmental clearance (central and periph 2)")
                 ("TVV4" "Typical peripheral 2 volume")
                 ("TVKA" "Typical absorption rate constant")))
            ( 6 "In terms of ALPHA, BETA, GAMMA, K32, K42, and KA"
                ";; ---------------------------------------------------------------- ;;
;; TRANS6                                                           ;;
;; ---------------------------------------------------------------- ;;
;;                                                                  ;;
;; ALPHA        = Alpha                                             ;;
;; BETA         = Beta                                              ;;
;; GAMMA        = Gamma                                             ;;
;; K32          = Rate constant (peripheral 1 to central)           ;;
;; K42          = Rate constant (peripheral 2 to central)           ;;
;; KA           = Absorption rate constant                          ;;
;;                                                                  ;;
;; Relationship:                                                    ;;
;;                                                                  ;;
;; K    = ALPHA*BETA*GAMMA/(K32*K42)                                ;;
;; K24  = (P+K42*K42-K42*S-K*K32)/(K32-K42)                         ;;
;; K12  = S-K-K24-K32-K42                                           ;;
;;                                                                  ;;
;; where                                                            ;;
;;                                                                  ;;
;; S    = ALPHA+BETA+GAMMA                                          ;;
;; P    = ALPHA*BETA+ALPHA*GAMMA+BETA*GAMMA                         ;;
;; KA is unchanged                                                  ;;
;;                                                                  ;;
;; Constraint:                                                      ;;
;;                                                                  ;;
;; Assuming that ALPHA<BETA<GAMMA, then  ALPHA<K32<BETA<K42<GAMMA   ;;
;;                                                                  ;;
;; or                                                               ;;
;;                                                                  ;;
;; ALPHA<K42<BETA<K32<GAMMA.                                        ;;
;;                                                                  ;;
;; The roles of ALPHA, BETA, GAMMA, K32, K42, K23,  and  K24  are   ;;
;; symmetric and are exchangeable.                                  ;;
;; ---------------------------------------------------------------- ;;"
                (
                 ("ALPHA" "Alpha")
                 ("BETA" "Beta")
                 ("GAMMA" "Gamma")
                 ("K32" "Rate constant (peripheral 1 to central)")
                 ("K42" "Rate constant (peripheral 2 to central)")
                 ("KA" "Absorption rate constant")
                 ("TVALPHA" "Typical alpha")
                 ("TVBETA" "Typical beta")
                 ("TVGAMMA" "Typical gamma")
                 ("TVK32" "Typical rate constant (peripheral 1 to central)")
                 ("TVK42" "Typical rate constant (peripheral 2 to central)")
                 ("TVKA" "Typical absorption rate constant")
                 ("TVV" "Central volume")
                 ("V" "Central volume")))))))
  
  "* Variables for ADVAN-TRANS-Combinations"
  :type '(repeat
          (list
           (integer :tag "ADVAN") ;0
           (string :tag "ADVAN Description (Small)") ;1
           (string :tag "ADVAN Description (Large)") ;2
           (repeat ;3
            (list
             (string :tag "ADVAN Variable")
             (string :tag "ADVAN Variable Description")))
           (repeat ;4
            (list
             (integer :tag "TRANS") ;0 
             (string :tag "TRANS Description (Small)") ;1
             (string :tag "TRANS Description (Large)") ;2
             (repeat ;3
              (list
               (string :tag "TRANS Variable")
               (string :tag "TRANS Variable Description")))))))
  :group 'esn-code)
;; 'languages

(defcustom esn-character-limit 80
  "Defines the maximum number of characters on a line (capacity of the NONMEM version running)."
  :type 'integer
  :group 'esn-general-options
  :group 'esn-80)
(provide 'esn-options)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-options.el ends here
