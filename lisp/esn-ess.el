;;; esn-ess.el --- Esn's ESS interface
;; 
;; Filename: esn-ess.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Sat Nov 13 21:45:22 2010 (-0600)
;; Version: 0.13
;; Last-Updated: Thu Sep  1 15:38:40 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 302
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM, Emacs Speaks Statistics
;; Compatibility: 23.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(eval-when-compile
  (require 'esn-vars))
(declare-function esn-input-data "esn-properties")
(declare-function esn-seq-concat "esn-templates")
(declare-function esn-rec "esn-properties")
(declare-function esn-plt-table-rep "esn-plt")
(declare-function esn-output-tables "esn-properties")
(declare-function esn-message "esn-exec")
(declare-function esn-get-rpt "esn-rpt-mode")
(declare-function esn-get-mod "esn-rpt-mode")
(declare-function esn-plt-find-timestamps "esn-plt")


(eval-when-compile
  (require 'esn-start)
  (require 'esn-reg))
(require 'ess-inf)

(defvar esn-R-started)
(defun esn-R (command)
  "Evaluates R command and returns results.  If multiple commands are specified, run them all."
  (let (
        (n (length ess-process-name-list))
        (ess-language "S")
        (ess-ask-for-ess-directory nil) ;; Don't prompt for a directory.  A tad intrusive.
        (tmpbuf (get-buffer-create " **esn-ess communication**"))
        ret)
    (ess-command (concat command "\n") tmpbuf)
    (save-excursion 
      (set-buffer tmpbuf)
      (setq ret (buffer-string))
      (kill-buffer tmpbuf))
    (symbol-value 'ret)))

;;;###autoload
(defun esn-R-list (cmd &optional force-string)
  "Gets R command and returns list of output.  Uses esn.lisp R function contained in esnR.

CMD R command to produce list.

FORCE-STRING boolean indicating that the output should be a list of strings even if it is numbers."
  (let (ret
        (r-cmd (concat "esn.lisp(" cmd (if (not force-string) ""
                                       ",force.string=TRUE")
                     ")")))
    (message "CMD: %s" r-cmd)
    (with-temp-buffer
      (insert "(setq ret ")
      (insert (esn-R r-cmd))
      (insert ")")
      (eval-buffer))
    (symbol-value 'ret)))


(defun esn-R-start ()
  "Starts R processes"
  (interactive)
  (if (esn-R "R.version")
      (progn
        (message "R process started")
        t)
    (message "R process was not started")
    nil))

(defun esn-R-create-list-cmd (list-name)
  "R code to create a list"
  (concat "if(!any(ls(all.names=TRUE)==\"" list-name "\")){" list-name " <- list()};"))

(defun esn-R-add-if-not-in-list-cmd (list-name what val)
  "R code to add to a list if the list doesn't already contain the item"
  (concat
   (esn-R-create-list-cmd list-name)
   "if (!any(names(" list-name ") ==  \"" what "\")){" list-name "[[\"" what "\"]] <- " val ";};"))

(defun esn-R-read-cmd (data &optional data-name function out-data extra-lab extra-args)
  "Unless already read in, read data into data-frame list.

DATA represents the path of the data.

DATA-NAME is the name of the R list that the data is being stored in.  Defaults to \".esn.inputs\".

FUNCTION is the method of reading in the data. Defaults to \"read.csv\".

OUT-DATA is the output dataset where the data is returned (in an R session). Defaults to \".d\".

EXTRA-LAB is the extra label added to the R storage list (in addition the the buffer file name).  Default \"\".

EXTRA-ARGS are extra arguments added to the read.data line.  Defaults to ,na.strings=c(\"NA\",\".\").  Requires the comma.
"
  (let (
        (dat (or data-name ".esn.inputs"))
        (fn (or function "read.csv"))
        (od (or out-data ".d"))
        (el (or extra-lab ""))
        (ea (or extra-args ",na.strings=c(\"NA\",\".\")"))
        )
    (concat (esn-R-add-if-not-in-list-cmd dat (concat (buffer-file-name) el) (concat fn "(\"" data "\"" ea ")"))
            od " <- " dat "[[\"" (buffer-file-name) el "\"]]")))

;;;###autoload
(defun esn-R-input-data ()
  "Loads data into R process.  Returns output on completion."
  (interactive)
  (when (eq major-mode 'esn-mode)
    (let (ret cmd
              (data (esn-get-abs-dir (esn-input-data))))
      (while (string-match "[\\\\]" data)
        (setq data (replace-match "/" nil nil data)))
      (cond
       ((string-match "csv$" data)
        (setq cmd (esn-R-read-cmd data))
        (setq ret (esn-R cmd))
        (esn-R-send-input-line)
        ))
      (symbol-value 'ret))))

(defun esn-R-load-esnR ()
  "Loads esnR package in R process for use in esn"
  (interactive)
  (esn-R
   (concat
    ".esnR <- TRUE;tryCatch(library(esnR),error=function(e) .esnR <<- FALSE);if (!esnR){install.packages(\""
    esn-path "etc/esnR_" (format "%s" esn-mode-ver) (if (memq system-type '(windows-nt))
                                                    ".zip"
                                                  ".tar.gz")
    "\");library(esnR)}")))

(defun esn-R-seq (n &optional quote function)
  "Create an R sequence
N is the sequence called
QUOTE is if the sequence should be quoted
FUNCTION is the function applied to each number."
  (concat "c(" (esn-seq-concat n (if quote "\"%s\"" "%s") "," function) ")"))

(defun esn-R-send-input-line ()
  "Sends Input line to R process"
  (save-excursion
    (save-restriction
      (let ((inp (esn-rec "INP" t t))
            (case-fold-search t)
            (start 0))
        (while (string-match (eval-when-compile (esn-reg-record-exp "INP" nil)) inp start)
          (setq start (match-beginning 0))
          (setq inp (replace-match "" nil nil inp)))
        ;; Drop all DROP and SKIP Statements
        (mapc (lambda(expr)
                (setq start 0)
                (while (string-match expr inp start)
                  (setq inp (replace-match "" nil nil inp))
                  (setq start (match-beginning 0))))
              (list "[ \t]*=[ \t]*\\(?:DROP\\|SKIP\\)\\>"
                    "\\<\\(DROP\\|SKIP\\)[ \t]*=[ \t]*"
                    "[ \t]*DROP\\>"))
        ;; Replace A = B with A=B
        (setq start 0)
        (while (string-match "[ \t]*=[ \t]*" inp start)
          (setq inp (replace-match "=" nil nil inp))
          (setq start (+ 1 (match-beginning 0))))
        (setq inp
              (esn-R-add-if-not-in-list-cmd
               ".esn.aliases"
               (buffer-file-name)
               (concat
                "list("
                (mapconcat
                 (lambda(itm)
                   (if (not (string-match "=" itm))
                       (concat "\"" itm "\"")
                     (concat "c(\"" (mapconcat (lambda(x) x) (split-string itm "=") "\",\"") "\")")))
                 (split-string inp) ",") ")")))
        (esn-R inp)))))

(defun esn-R-output-data ()
  "Loads output data into R process"
  (interactive)
  (when (esn-R-load-esnR)
    (let (;(debug-on-error t)
          (out
           (mapcar
            (lambda(x)
              (let ((ret (esn-plt-table-rep x)))
                (while (string-match "[\\\\]" ret)
                  (setq ret (replace-match "/" nil nil ret)))
                (symbol-value 'ret)))
            (esn-output-tables)))
          cmd)
      (mapc (lambda(file)
              (esn-message "Loading data into R: %s" file)
              (setq cmd (esn-R-read-cmd file ".esn.outputs" "read.nm" nil file))
              (esn-R cmd))
            out)
      (esn-R-input-data))))

(defun esn-R-reinstall-esnR ()
  "Reinstalls esnR"
  (interactive)
  (esn-R
   (concat "try(detach(package:esnR));try(install.packages(\""
           esn-path "etc/esnR_" (format "%s" esn-mode-ver)
           (if (memq system-type '(windows-nt))
               ".zip"
             ".tar.gz")
           "\"));library(esnR);")))

(defun esn-R-quote (val)
  "Quotes Arguments as strings in R.

VAL is the string to be quoted."
  (let ((ret val)
        (start 0))
    (while (string-match "\"" ret start)
      (setq ret (replace-match "\\\"" t t ret))
      (setq start (+ 2 (match-beginning 0))))
    (setq ret (concat "\"" ret "\""))
    (symbol-value 'ret)))

(defun esn-R-send-input-output-file-names ()
  "Sends input and output file names"
  (let ((lst (esn-get-rpt))
        (mod (esn-get-mod)))
    (and 
     (esn-R (esn-R-add-if-not-in-list-cmd ".ctl" (buffer-file-name) (esn-R-quote mod)))
     (esn-R (esn-R-add-if-not-in-list-cmd ".lst" (buffer-file-name) (esn-R-quote lst))))))

(defalias 'e 'esn-R-get-sum)

(defun esn-R-get-sum ()
  "Gets summary of run and stores in R list."
  (interactive)
  (when (esn-R-send-input-output-file-names)
    (message "%s" (esn-R 
             (esn-R-add-if-not-in-list-cmd ".s" (buffer-file-name)
                                           (format "getSum(lst.file=.lst[[\"%s\"]],ctl.file=.ctl[[\"%s\"]])"
                                                   (buffer-file-name) (buffer-file-name)))))))

(defun esn-R-xpose4-p ()
  "Determines if Xpose4 is installed in R."
  (esn-R ".xpose4 <- TRUE; tryCatch(library(xpose4),error=function(e) .xpose4 <<- FALSE);")
  (if (string-match "TRUE" (esn-R ".xpose4;"))
      t
    nil))

(defalias 'e 'esn-R-get-xpose4)
(defun esn-R-get-xpose4 ()
  "Puts ouput data into an xpose object"
  (interactive)
  (when (and (esn-R-xpose4-p) (esn-R-load-esnR))
    (cond
     ((esn-use-plt-p)
      (let ((timestamps (esn-plt-find-timestamps)))
        (when timestamps
          (message "%s" (esn-R (esn-R-add-if-not-in-list-cmd ".xpdb" (buffer-file-name)
                                        (format "xpose.plt(\"%s\")" (nth 0 timestamps)))))))))))



(provide 'esn-ess)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-ess.el ends here
