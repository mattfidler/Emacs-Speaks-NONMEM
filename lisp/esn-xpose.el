;;;; esn-xpose.el --- Xpose support and Table updating support
;;
;; Filename: esn-xpose.el
;; Description: Xpose and Table updating support
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Mon Feb  1 16:13:55 2010 (-0600)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 519
;; URL: esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM, Xpose Tables
;; Compatibility: 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Xpose tables and other table functions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 06-Dec-2010    Matthew L. Fidler  
;;    Bug fix for tables -- was deleting items like PPRED when it should be keeping these items if defined.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Bug fix to `esn-xpose-get-input-line'
;; 04-Oct-2010    Matthew L. Fidler  
;;    Bug fix -- When TVx is not requested, but x is requested, remove the TVx from the output table.
;; 14-Sep-2010    Matthew L. Fidler
;;    Took out FORMAT, ESAMPLE and SEED from calculating what variables are within a table.
;; 01-Sep-2010    Matthew L. Fidler
;;    Made xpose table skip empty regular expressions (bugfix)
;; 01-Sep-2010    Matthew L. Fidler
;;    Changed Xpose table regular expression to be words by default (bugfix)
;; 31-Aug-2010    Matthew L. Fidler  
;;    Revised esn-xpose-get-input-line
;; 30-Aug-2010    Matthew L. Fidler  
;;    Updated esn-xpose-get-all-variables to ensure that it is only looking at
;;    abbreviated records.
;; 27-Aug-2010    Matthew L. Fidler  
;;    Minor bug fixes.
;; 20-Aug-2010    Matthew L. Fidler  
;;    Bug fix for esn-xpose-insert-table based on standardized expressions
;; 20-Aug-2010    Matthew L. Fidler  
;;    Bug fix esn-xpose-get-all-variables based on standardized expressions
;;    changing.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Tried to standardize regular expressions.
;; 13-Jul-2010    Matthew L. Fidler  
;;
;;    Changed Table update to only generate information about selected tables,
;;    not every table possible
;;
;; 13-Jul-2010    Matthew L. Fidler  
;;    Added esn-xpose-get-input-line-save to speed up saves.
;; 13-Jul-2010    Matthew L. Fidler  
;;    Changed esn-xpose-patab-vars to only keep A0 when A0=TVA0.
;; 13-Jul-2010    Matthew L. Fidler  
;;    Changed esn-xpose-get-all-variables to only add A0 when A0=TVA0...
;; 13-Jul-2010    Matthew L. Fidler
;;    Make variable routine only look at non-commented portion of code
;; 12-Jul-2010    Matthew L. Fidler
;;    Changed so that CWRES is not generated
;; 25-Jun-2010    Matthew L. Fidler
;;    Added inter-occasion handling for generating tables
;; 17-Jun-2010    Matthew L. Fidler
;;    Added per-buffer saving of using Xpose
;; 04-May-2010    Matthew L. Fidler
;;    Added support for table.eta and table.par in PDx pop.
;; 04-May-2010    Matthew L. Fidler
;;    Added PDx pop run-number of "" when there is no detectable run in file.
;; 02-Apr-2010    Matthew L. Fidler
;;
;;    Fixed updating FirstRecords.txt to include ID variables when included in
;;    AllRecords.txt
;;
;; 01-Feb-2010    Matthew L. Fidler
;;    Fixed updating FirstRecords.txt with ETAx when it is present in AllRecords.txt
;; 01-Feb-2010    Matthew L. Fidler
;;    Added Header.
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

(declare-function esn-narrow-to-current-problem "esn-narrow")


;; Implements Xpose Specific features as well as all automatic tables
;; and file naming conventions.

(require 'esn-vars) 
(require 'esn-options)

(defun esn-get-extension (ext &optional file)
  "This returns the \"extension\" for Xpose when not Xpose naming conventions for control streams are not enforced."
  (save-match-data
    (if (not esn-xpose-tables-for-non-xpose-ctl-streams)
        (progn
          (if (string= "run" (downcase ext))
              ""
            (symbol-value 'ext)))
      (let (
            (case-fold-search 't)
            (fn (or file (buffer-file-name))))
        (when (string-match (format "%s$" (regexp-opt esn-default-extension)) fn)
          (setq fn (replace-match "" nil nil fn)))
        (when (string-match "[-_~]?[0-9]*$" fn)
          (setq fn (replace-match "" nil nil fn)))
        (when (string-match "[\\/]\\([^\\/]*\\)$" fn)
          (setq fn (match-string 1 fn)))
        (setq fn (concat fn ext))
        (symbol-value 'fn)))))

(defun esn-xpose-run-number (&optional file)
  "* Returns the run number."
  (let ((case-fold-search 't)
  	(fn (or file (buffer-file-name)))
	(run nil)
        (padding (or esn-xpose-choose-file-name-padding esn-xpose-choose-file-name-no-run
                     esn-pdx-choose-file-name-padding)))
    (if (not fn)
        (setq run nil)
      (if (string-match (format "[/\\\\]\\(?:run\\)?\\([0-9]+\\)%s"
                                (regexp-quote
                                 (if (esn-use-pdx-p)
                                     ".ctl"
                                   esn-xpose-default-extension))) fn)
          (setq run (match-string 1 fn))
        (when esn-xpose-tables-for-non-xpose-ctl-streams
          (if (string-match (format "\\([0-9]+\\)%s$"
				    (regexp-opt esn-default-extension)) fn)
              (progn
                (setq run (match-string 1 fn)))
            (if (esn-use-pdx-p)
                (setq run "")
              (setq run "0"))))))
    (symbol-value 'run)))

(defun esn-xpose-get-all-variables ()
  "* Gets every variable."
  (let ((varlst '("DV" "PRED" "WRES" "RES" "MDV"))
        (ret "")
        (case-fold-search 't)
        (orphan-tvs '())
        (mu-tvs '())
        tmp
        tmp-mu
        predpk
        (debug-on-error 't))
    (setq predpk "")
    (mapc
     (lambda(x)
       (setq predpk (concat predpk (esn-rec x 't) "\n")))
     esn-current-abbrev-records)
    (with-temp-buffer
      (insert predpk)
      (goto-char (point-min))
      (while (re-search-forward ";.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward (eval-when-compile (esn-reg-records 't)) nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*\\([A-Z]\\w*\\)[ \t]*=" nil t)
        (setq tmp (esn-rec3 (match-string 1))) 
        (when (save-match-data (looking-at (format "TV%s[ \t]*$" tmp)))
          (add-to-list 'orphan-tvs (concat "TV" (upcase (match-string 1)))))
        (when (and (save-match-data (if (looking-at ".*\\<\\(MU_[0-9]+\\)\\>.*$")
                                        (progn
                                          (setq tmp-mu (match-string 1))
                                          't
                                          )
                                      nil))
                   (not (save-match-data (looking-at ".*\\<ETA([0-9]+).*$")))
                   (save-match-data (string-match "TV.*$" tmp)))
          (add-to-list 'mu-tvs tmp-mu))
        (add-to-list 'varlst (upcase (match-string 1))))
      (unless (= 0 (length orphan-tvs))
        (setq tmp (downcase (format "^%s$" (regexp-opt orphan-tvs 't))))
        (setq orphan-tvs '())
        (mapc (lambda (x)
                (unless (string-match tmp (downcase x))
                  (add-to-list 'orphan-tvs x)))
              varlst
              )
        (setq varlst orphan-tvs))
      (unless (= 0 (length mu-tvs))
        (setq tmp (downcase (format "^%s$" (regexp-opt mu-tvs 't))))
        (setq orphan-tvs '())
        (mapc (lambda(x)
                (unless (string-match tmp (downcase x))
                  (add-to-list 'orphan-tvs x)))
              varlst)
        (setq varlst orphan-tvs))
      (setq ret (regexp-opt orphan-tvs 'words))
      (symbol-value 'ret))))

;;;###autoload
(defun esn-xpose-get-input-line (&optional current no-reg)
  "* Gets a regular expression denoting all the acceptable input
line parameters.  This is done for each problem.  If no input
line is present, then return \"\"

When current is non-nil, return information from the current problem.

If no-reg is non-nil, then return the input line list.
"
  (interactive)
  (if (and (not current)
           (not no-reg)
           esn-xpose-get-input-line-save) ; Should be cleared after something is done in $INP record. 
      esn-xpose-get-input-line-save
    (save-restriction
      (let ((case-fold-search 't)
            (np (esn-num-problems))
            (i 0)
            (regs '())
            inp)
        (save-excursion
          (while (< i np)
            (if current
                (progn
                  (esn-narrow-to-current-problem)
                  (setq i (+ 1 np)) ;; Exit loop after one iteration.
                  )
              (setq i (+ i 1))
              (esn-narrow-to-problem i))
            (setq inp (esn-rec "INP" 't))
            (with-temp-buffer
              (insert inp)
              (goto-char (point-min))
              (while (re-search-forward ";.*" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (while (re-search-forward (esn-reg-records 't 't) nil t)
                (replace-match ""))
              (while (re-search-forward "\\(DROP\\|SKIP\\)[ \t]*=[ \t]*[A-Za-z][A-Za-z0-9_]*\\>" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (while (re-search-forward "\\<[A-Za-z][A-Za-z0-9_]*[ \t=]+\\(DROP\\|SKIP\\)" nil t)
                (replace-match ""))
              (when current
                (if no-reg
                    (setq regs (split-string (buffer-substring (point-min) (point-max)) "[ \t=]+" 't))
                  (setq regs (regexp-opt (split-string (buffer-substring (point-min) (point-max)) "[ \t=]+" 't) 'words))))
              (unless current
                (if no-reg
                    (setq regs (push (split-string (buffer-substring (point-min) (point-max)) "[ \t=]+" 't) regs))
                  (setq regs (push (regexp-opt (split-string (buffer-substring (point-min) (point-max)) "[ \t=]+" 't) 'words) regs)))))
            (widen)))
        (unless current
          (setq regs (reverse regs)))
        (when (and (not current) (not no-reg))
          (setq esn-xpose-get-input-line-save regs))
        (symbol-value 'regs)))))



(defun esn-pdx-eta-tab ()
  "* Defines PDx Eta tab"
  (list
   (list
    (concat (esn-runname-noext 't) ".eta")
    (esn-xpose-get-suitable-etas)
    nil
    nil nil nil)))

(defun esn-one-tab (cwres)
  "* Defines variables in a single table"
  (list
   (list
    (concat
     (esn-runname-noext 't)
     (if (esn-use-pdx-p)
         ".tab"
       esn-table-extension))
    (append ;List of variables that should be updated.
     esn-xpose-tbl
     (when esn-update-table-continuous-covariates
       esn-xpose-cotab)
     (when esn-update-table-categorical-covariates
       esn-xpose-catab)
     (when esn-update-table-mutab
       esn-xpose-mutab)
     (when esn-update-table-extra
       esn-xpose-extra)
     (when esn-update-table-sdtab
       esn-xpose-sdtab)
     (when esn-update-table-patab
       (esn-xpose-patab-vars))
     (when esn-update-table-cwtab
       cwres)
     (when esn-update-table-mytab
       esn-xpose-mytab)
     (when esn-update-table-xptab
       esn-xpose-xptab))
    (append
     esn-xpose-no-tbl
     (when esn-update-table-continuous-covariates
       esn-xpose-no-cotab)
     (when esn-update-table-categorical-covariates
       esn-xpose-no-catab)
     (when esn-update-table-mutab
       esn-xpose-no-mutab)
     (when esn-update-table-extra
       esn-xpose-no-extra)
     (when esn-update-table-sdtab
       esn-xpose-no-sdtab)
     ;; Parameters should be ignored.
     ;; (when esn-update-table-patab
     ;; (esn-xpose-patab-vars))
     ;; CWRES should be ignored.
     ;; (when esn-update-table-cwtab
     ;; cwres)
     (when esn-update-table-mytab
       esn-xpose-no-mytab)
     (when esn-update-table-xptab
       esn-xpose-no-xptab))
    nil nil nil)))

(defun esn-xpose-cwtab-vars (&optional no-id nmv)
  "* Inserts the cwtab variables"
  (if (not esn-automatically-generate-cwres)
      '()
    (let (
          (var (if no-id '() esn-xpose-cwtab))
          (inhibit-read-only 't)
          (inhibit-point-motion-hooks 't)
          (case-fold-search 't)
          (nm-ver (or nmv (esn-update-get-version)))
          (added nil))
      (when (string= "-1" nm-ver)
        (setq nm-ver esn-assumed-version))
      (if (< (string-to-number nm-ver) 6.2)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "^\" +COM(\\([0-9]+\\)) *= *\\(G\\|H\\)H?(\\([0-9]+\\) *\\, *1 *)" nil t)
              (add-to-list 'var (format "COM(%s)=%s%s1" (match-string 1) (match-string 2) (match-string 3)))
              (setq added 't))
            (unless added
              (setq var '())))
        (save-excursion
          (if (not (or
                    (and esn-cwres-foce (esn-is-foce-p))
                    (and esn-cwres-focei (esn-is-focei-p))
                    (and esn-cwres-hybrid (esn-is-hybrid-p))
                    (and esn-cwres-fo-posthoc (esn-is-fo-posthoc-p))
                    (and esn-cwres-foi-posthoc (esn-is-foi-posthoc-p))
                    (and esn-cwres-lap (esn-is-lap-p))))
              (setq var '())
            (let (
                  (maxeta (esn-max-eta))
                  (maxeps (esn-max-eps))
                  (i 0))
              (while (< i maxeta)
                (setq i (+ i 1))
                (add-to-list 'var (format "G%s1" i)))
              (setq i 0)
              (while (< i  maxeps)
                (setq i (+ 1 i))
                (add-to-list 'var (format "H%s1" i)))))))
      (symbol-value 'var))))

(defun esn-get-pwd ()
  "Gets the pwd of the file."
  (file-name-directory (buffer-file-name)))

(defun esn-xpose-fish-run-number ()
  "* Fishes for a version that is not already there..."
  (let ((cver 1)
        (pwd (esn-get-pwd))
        (padding (or esn-xpose-choose-file-name-padding
                     esn-xpose-choose-file-name-no-run-padding
                     esn-pdx-choose-file-name-padding
                     ))
        (run (if (or esn-xpose-choose-file-name-no-run
                     esn-xpose-choose-file-name-no-run-padding
                     esn-pdx-choose-file-name
                     esn-pdx-choose-file-name-padding
                     )
                 ""
               "run"))
        (file ""))
    (setq file (format "%s%s%s%s"
                       pwd
                       run
                       (let ((tmp (number-to-string cver)))
                         (when padding
                           (while (< (length tmp) 3)
                             (setq tmp (concat "0" tmp))))
                         (symbol-value 'tmp))
                       (if (esn-use-pdx-p)
                           ".ctl"
                         esn-xpose-default-extension)))
    (while (file-exists-p file)
      (message "Found %s,looking for another appropriate name." file)
      (setq cver (+ cver 1))
      (setq file (format "%s%s%s%s"
                         pwd
                         run
                         (let ((tmp (number-to-string cver)))
                           (when padding
                             (while (< (length tmp) 3)
                               (setq tmp (concat "0" tmp))))
                           (symbol-value 'tmp))
                         (if (esn-use-pdx-p)
                             ".ctl"
                           esn-xpose-default-extension))))
    (setq cver (number-to-string cver))
    (when padding
      (while (< (length cver) 3)
        (setq cver (concat "0" cver))))
    (symbol-value 'cver)))

(defun esn-xpose-save-run-number (num)
  "* A save function that saves the file appropriately to a new run."
  (let ((inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (fn (buffer-file-name))
        (cver (number-to-string num))
        (pwd (esn-get-pwd))
        (case-fold-search 't)
        (nm (number-to-string num))
        (padding (or esn-xpose-choose-file-name-padding
                     esn-xpose-choose-file-name-no-run-padding
                     esn-pdx-choose-file-name-padding
                     ))
        (run (if (or esn-xpose-choose-file-name-no-run
                     esn-xpose-choose-file-name-no-run-padding
                     esn-pdx-choose-file-name-padding
                     esn-pdx-choose-file-name
                     )
                 ""
               "run"
               )))
    (when padding
      (while (< (length nm) 3)
        (setq nm (concat "0" nm)))
      (while (< (length cver) 3)
        (setq cver (concat "0" cver))))
    (if (not (file-exists-p (format "%s%s%s%s" pwd run nm (if (esn-use-pdx-p) ".ctl" esn-xpose-default-extension)))) nil
      (if (not (file-exists-p (format "%s%s%s%s" pwd run nm (if (esn-use-pdx-p) ".res" esn-xpose-default-output))))
          (if (and
               (not (string-match (regexp-quote (format "%s%s%s%s" run pwd nm (if (esn-use-pdx-p) ".ctl" esn-xpose-default-extension))) fn))
               (file-exists-p
                (format "%s%s%s%s" pwd run
                        nm
                        (if (esn-use-pdx-p) ".ctl" esn-xpose-default-extension))))
              (progn
                (setq cver (esn-xpose-fish-run-number))))
        (if (file-newer-than-file-p
             (format "%s%s%s%s" pwd run nm (if (esn-use-pdx-p) ".ctl" esn-xpose-default-extension))
             (format "%s%s%s%s" pwd run nm (if (esn-use-pdx-p) ".res" esn-xpose-default-output))) nil
          (if (y-or-n-p (format "Run %s exists, and successfully sumbited. Create new run? " nm))
              (save-excursion
                
                (setq cver (esn-xpose-fish-run-number)))
            ;; Version control it.
            ;; Before save use RCS to add the previous version to the RCS log (via
            ;; command line)
            (when 't
              (setq esn-commit-last-version 't))))))
    (setq cver (format "%s%s%s%s" pwd run cver
                       (if (esn-use-pdx-p)
                           ".ctl"
                         esn-xpose-default-extension)))
    (symbol-value 'cver)))

(defun esn-xpose-save-to-other-file (fn)
  "* Saves the str to a new file, reverts and kills current file."
  (let (
        (pt (point))
        (str (buffer-substring (point-min) (point-max)))
        (ofn (buffer-file-name))
        (inhibit-read-only 't))
    (unless (string= ofn fn)
      (text-mode)
      (if (not (file-exists-p ofn)) nil
        (revert-buffer 't 't)
        (kill-buffer (current-buffer)))
      (find-file fn)
      (delete-region (point-min) (point-max))
      (insert str)
      (setq esn-last-full-name "should be a copy.")
      ;;    (message "About to save to a new name...")
      (save-buffer)
      (esn-mode)
      (goto-char pt))))

;;;###autoload
(defun esn-xpose-save ()
  "* Makes sure that xpose naming conventions for model files are correct. Returns if additional update hooks should be stopped."
  (unless esn-inhibit-xpose
    (if (or
         (and esn-xpose
              (or
               esn-xpose-choose-file-name
               esn-xpose-choose-file-name-padding
               esn-xpose-choose-file-name-no-run-padding
               esn-xpose-choose-file-name-no-run
               )
              (not (esn-use-plt-p))
              (not (esn-use-pdx-p)))
         (and
          (esn-use-pdx-p)
          (or
           esn-pdx-choose-file-name
           esn-pdx-choose-file-name-padding
           )))
        (let (
              (bn (buffer-file-name))
              (run "")
              (stop 't)
              (case-fold-search 't)
              (esn-force-pdx (esn-use-pdx-p)))
          (if (not (string-match "\\(?:run\\)?\\([0-9]+\\)" bn))
              (progn
                (esn-xpose-save-to-other-file (esn-xpose-save-run-number 1)))
            (setq run (esn-xpose-save-run-number (string-to-number (match-string 1 bn))))
            (if (string-match (regexp-quote run) bn)
                (setq stop nil)
              (esn-xpose-save-to-other-file run)))
          (symbol-value 'stop))
      nil)))



;; Xpose tables

(require 'esn-tables)
(defun esn-xpose-rename-old-table (table-name run)
  "Renames old TABLE-NAME to new TABLE-NAME"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (format "\\<FILE[ \t]*=?[ \t]*\\(\\(?:%s\\|%s\\).*?\\)\\>" (regexp-quote table-name)
                    (regexp-quote (substring table-name 0 2))) nil t)
      (let ((new-run (format "%s%s"
                             (if esn-xpose-small-table-file-names
                                 (substring table-name 0 2)
                               table-name) run)))
        (when (and (string= (esn-get-current-rec) "TAB")
                   (not (string= new-run (match-string 1))))
          (delete-region (match-beginning 1) (match-end 1))
          (insert (format "%s%s"
                          (if esn-xpose-small-table-file-names
                              (substring table-name 0 2)
                            table-name) run)))))))

(defmacro esn-xpose-gen-table-functions (table-name)
  "Defines table functions for xpose"
  `(defun ,(intern (concat "esn-xpose-" table-name "-table-name")) ()
     ,(concat "Gets the table name for " table-name " tables in Xpose")
     (let ((run (esn-xpose-run-number)))
       (esn-xpose-rename-old-table ,table-name run)
       (concat
        (if esn-xpose-small-table-file-names
            ,(substring table-name 0 2)
          ,table-name)
        run
        ;; esn-xpose-data-sim-extension used to be there.
        ))))
(eval-when-compile
  (mapc
   (lambda(x)
     (eval (macroexpand `(esn-xpose-gen-table-functions ,x))))
   '("cotab" "catab" "patab" "sdtab")))

(esn-deftable esn-xpose-catab
  :table-name esn-xpose-catab-table-name
  :table-options "NOPRINT ONEHEADER"
  :group 'esn-xpose-tables
  :condition (and esn-xpose-generate-tables (esn-use-xpose-p) (esn-rec "EST"))
  :require-categorical t
  :require-race t
  :require-gender t
  :require-dose t
  :require-fed t
  :require-group t
  :require-race t
  :add-id t)

(esn-deftable esn-xpose-cotab
  :table-name esn-xpose-cotab-table-name
  :table-options "NOPRINT ONEHEADER"
  :group 'esn-xpose-tables
  :condition (and esn-xpose-generate-tables (esn-use-xpose-p) (esn-rec "EST"))
  :require-continuous t
  :require-age t
  :require-height t
  :add-id t)

(esn-deftable esn-xpose-patab
  :table-name esn-xpose-patab-table-name
  :table-options "NOPRINT ONEHEADER"
  :group 'esn-xpose-tables
  :condition (and esn-xpose-generate-tables (esn-use-xpose-p) (esn-rec "EST"))
  :require-par-iov t
  :require-population t
  :require-individual t
  :require-eta t
  :add-id t)

(esn-deftable esn-xpose-sdtab
  :table-name esn-xpose-sdtab-table-name
  :table-options "NOPRINT ONEHEADER"
  :group 'esn-xpose-tables
  :condition (and esn-xpose-generate-tables (esn-use-xpose-p) (esn-rec "EST"))
  :require-par-res t
  :require-sdtab t
  :add-id t)


(provide 'esn-xpose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-xpose.el ends here
