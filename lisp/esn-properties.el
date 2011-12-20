;;; esn-properties.el --- Provides properties about the current control Stream.
;; 
;; Filename: esn-properties.el
;; Description: Provides properties about the current control Stream.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Filder
;; Created: Thu Jan 28 15:58:26 2010 (-0600)
;; Version: Provides properties about the current control Stream.
;; Last-Updated: Mon May  2 13:08:15 2011 (-0500)
;;           By: Matthew L. Fidler
;;     Update #: 279
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM 
;; Compatibility: Emacs 23.x
;;
;; Features that might be required by this library:
;;
;;   `esn-autoloads', `esn-macros', `esn-magic-keys', `esn-options',
;;   `esn-rec-hooks', `esn-reg', `esn-start', `esn-vars'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; Properties of the control stream
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 18-Jan-2011      
;;    Last-Updated: Tue Jan 18 11:11:35 2011 (-0600) #273 (us041375)
;;    Added `esn-get-fixed-thetas' for font-lock.
;; 22-Dec-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Dec 22 14:19:45 2010 (-0600) #243 (Matthew L. Fidler)
;;    Added `esn-is-foi-p'
;; 13-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Nov 13 20:12:30 2010 (-0600) #164 (Matthew L. Fidler)
;;    Change `esn-in-comment' to understand $PROBLEM statements.
;; 13-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Sat Nov 13 16:08:20 2010 (-0600) #157 (Matthew L. Fidler)
;;    Added `esn-wfn-runname'
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 10:19:01 2010 (-0500) #134 (Matthew L. Fidler)
;;    Changed `esn-in-comment' to be less complicated.
;; 03-Nov-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Nov  3 09:45:09 2010 (-0500) #123 (Matthew L. Fidler)
;;
;;    I'm making the assumption that not only beginning a line with a C causes a
;;    comment but begining a line with * or ! begins a comment in NONMEM 7.
;;    `esn-in-comment' has been updated to reflect this.
;;
;; 21-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Sep 21 13:28:56 2010 (-0500) #121 (Matthew L. Fidler)
;;    Made max theta, eps and eta have an option of "no extended"
;; 16-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Sep 16 10:27:18 2010 (-0500) #111 (Matthew L. Fidler)
;;    Allowed destructive option to be a string to replace region with.
;; 15-Sep-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Sep 15 15:58:25 2010 (-0500) #106 (Matthew L. Fidler)
;;    Added destructive option to esn-rec.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 11:36:00 2010 (-0500) #91 (Matthew L. Fidler)
;;    Tried to standardize record regular expressions.
;; 19-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Aug 19 09:01:22 2010 (-0500) #79 (Matthew L. Fidler)
;;    Bug fixed esn-rec to take records with that do not begin on a new line.
;; 11-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 11 14:31:46 2010 (-0500) #76 (Matthew L. Fidler)
;;    Revised esn-current-record and move to esn-narrow.
;; 11-Aug-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Aug 11 12:11:42 2010 (-0500) #73 (Matthew L. Fidler)
;;    Added esn-problem-number to get the problem number of the record.
;; 30-Jun-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Jun 30 08:56:54 2010 (-0500) #68 (Matthew L. Fidler)
;;    Revised esn-rec to better use save-restriction.
;; 10-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Feb 10 23:17:56 2010 (-0600) #57 (Matthew L. Fidler)
;;
;;    Changed get-current-record to check to make sure the buffer length is the
;;    same.
;; 
;; 10-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Feb 10 19:40:57 2010 (-0600) #27 (Matthew L. Fidler)
;;    Add another speed update to get-current-record
;; 10-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Wed Feb 10 08:40:52 2010 (-0600) #21 (Matthew L. Fidler)
;;    Added speed update to get-current-record (saves information)
;; 09-Feb-2010    Matthew L. Fidler  
;;    Last-Updated: Tue Feb  9 22:27:14 2010 (-0600) #10 (Matthew L. Fidler)
;;
;;    Change get-current-record to ignore INPUT Variables comment (speeds up
;;    spacing of INPUT.
;;
;; 28-Jan-2010    Matthew L. Fidler  
;;    Last-Updated: Thu Jan 28 15:59:08 2010 (-0600) #1 (Matthew L. Fidler)
;;    Added Text properties for Xmind summary file update.
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

(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-get-variable-names "esn-extended")
(declare-function esn-fix-numbering "esn-extended")
(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-update-get-purpose "esn-update")
(declare-function esn-narrow-to-current-problem "esn-narrow")
(eval-when-compile
  (require 'esn-reg))
(require 'esn-macros)
(defvar esn-fixed-theta-p-cache nil
  "Cache for esn-fixed-theta-p")
(make-variable-buffer-local 'esn-fixed-theta-p-cache)

;; Clear cache on $THETA modification.
(esn-rec-modification-hook "theta" (lambda() (setq esn-fixed-theta-p-cache nil)))

;;;###autoload
(defun esn-fixed-theta-p (theta-number)
  "Checks to see if THETA-NUMBER is fixed in the $THETA block.  Only considers the first problem."
  (let ((n (if (stringp theta-number)
               (string-to-number theta-number)
             (if (numberp theta-number)
                 theta-number
               (error "theta-number must be a string or number.")))))
  (if esn-fixed-theta-p-cache
      (if (not (assoc n esn-fixed-theta-p-cache)) nil
        (nth 1 (assoc n esn-fixed-theta-p-cache)))
  (save-match-data
    (let ((theta (esn-rec "THE" t t))
          (start 0)
          (case-fold-serach t)
          (est "\\(?:([^)]*)\\|[0-9.]+\\)\\([ \t]?FIX\\(?:ED?\\)?\\)?")
          ret)
      (while (string-match ";.*" theta start)
        (setq start (match-beginning 0))
        (setq theta (replace-match "" t t theta)))
      (setq start 0)
      (dotimes (i (esn-max-theta) ret)
        (if (not (string-match est theta start))
            (progn
              (setq ret nil)
              (add-to-list 'esn-fixed-theta-p-cache (list (+ i 1) nil))
              )
          (setq start (match-end 0))
          (setq ret (string-match "\\<FIX\\(?:ED?\\)?\\>" (match-string 0 theta)))
          (add-to-list 'esn-fixed-theta-p-cache (list (+ i 1) ret))))
      (esn-fixed-theta-p n))))))

;;;###autoload
(defun esn-nm-ver>= (version)
  "Is the control stream using NONMEM VERSION or greater?"
  (let (
        (nm-ver (esn-update-get-version))
        )
    (when (string= "-1" nm-ver)
      (setq nm-ver esn-assumed-version))
    (setq nm-ver (string-to-number nm-ver))
    (>= nm-ver version)))

;;;###autoload
(defun esn-output-tables ()
  "Gets a list of the output tables in the current control-stream"
  (interactive)
  (let ((lst '()))
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (while (re-search-forward (eval-when-compile (esn-reg-record-exp "TAB" t t)) nil t)
          (save-restriction
            (esn-narrow-rec)
            (if (re-search-forward "\\<FILE[ \t]*[= ]+[ \t]*\\(['\"]\\)\\(.*?\\)\\1\\>" nil t)
                (add-to-list 'lst (match-string 2))
              (if (re-search-forward "\\<FILE[ \t]*[= ]+[ \t]*\\([^ \t\n]*\\)" nil t)
                  (add-to-list 'lst (match-string 1))))))))
    (symbol-value 'lst)))

;;;###autoload
(defun esn-input-data ()
  "Provides the current control stream's input dataset"
  (save-excursion
    (goto-char (point-min))
    (if (not (re-search-forward (eval-when-compile (esn-reg-data-rec)) nil t)) nil
      (match-string 2))))

;;;###autoload
(defun esn-runname-noext (&optional nospaces)
  "Gets run-name, based on the current buffer.
According to WFN a runname is the file without the extension.
Also accoring to WFN you need to be in the current directory to execute it.
When nospaces is true, replace spaces with _.
"
  (let (
        (fn (buffer-file-name)))
    (if fn
        (setq esn-wfn-runname-last-bfn fn)
      (setq fn esn-wfn-runname-last-bfn))
    (if (string-match "\\.[^.]*$" fn)
        (setq fn (replace-match "" nil nil fn)))
    (if (string-match "[\\\\/]\\([^\\\\/]*\\)$" fn)
        (setq fn (match-string 1 fn)))
    (when nospaces
      (while (string-match " " fn)
        (setq fn (replace-match "_" nil nil fn))))
    (symbol-value 'fn)))

;;;###autoload
(defun esn-problem-number (&optional point)
  "* Figures out the problem number for the current point.
Problem 1 is the first problem"
  (let (
        (n 0)
        (case-fold-search 't))
    (save-excursion
      (save-match-data
        (when point
          (goto-char point))
        (while (re-search-backward (eval-when-compile (esn-reg-record-exp "PRO" 't)) nil t)
          (setq n (+ n 1)))))
    (symbol-value 'n)))

;;;###autoload
(defun esn-eta-error (&optional the ome sig)
  "* Returns the errors for ETAs"
  (interactive)
  (let (
        (bf (esn-standardize-lines the ome sig))
        (i 0)
        (lst '())
        (tmp ""))
    ;; Determines if the variability is:
    ;; * Additive
    ;; * Proportional
    ;; * Exponential
    ;; * IOV if coded in a specific way.
    ;; * ? if cannot determine
    (while (< i (esn-max-eta))
      (setq i (+ i 1))
      (cond
       ( (string-match (format "\\*\\<EXP(\\(\\(IOV\\|MU\\)[A-Za-z0-9_]+\\+\\)*ETA(%s)\\(\\+\\(IOV\\|MU\\)[A-Za-z0-9_]+\\)*)" i) bf)
         (push "EXP" lst))
       ( (string-match (format "\\*(1\\+ETA(%s))" i) bf)
         (push "PROP" lst))
       ( (string-match (format "=\\(\\(TV\\|MU\\)[A-ZA-z0-9_]+\\|THETA([0-9]+)\\)\\+ETA(%s)$" i) bf)
         (push "ADD" lst))
       ( (string-match (format "=\\(\\(TV\\|MU\\)[A-ZA-z0-9_]+\\|THETA([0-9]+)\\)\\+ETA(%s)$" i) bf)
         (push "ADD" lst))
       ( (string-match (format "=ETA(%s)\\+\\(\\(TV\\|MU\\)[A-ZA-z0-9_]+\\|THETA([0-9]+)\\)$" i) bf)
         (push "ADD" lst))
       ( (string-match (format "=ETA(%s)\\+\\(\\(TV\\|MU\\)[A-ZA-z0-9_]+\\|THETA([0-9]+)\\)$" i) bf)
         (push "ADD" lst))
       ( (string-match (format "\\(IOV[A-Za-z0-9_]\\)=.*\\<ETA(%s).*" i) bf)
         (setq tmp (match-string 1 bf))
         (cond
          ( (string-match (format "=.*EXP(.*?%s.*?).*" tmp) bf)
            (push "IOV,EXP" lst))
          ( 't
            (push "IOV" lst))))
       ( 't
         (push "?" lst))))
    (setq lst (reverse lst))
    (symbol-value 'lst)))

;;;###autoload
(defun esn-standardize-lines (&optional the ome sig do-theta)
  "* Returns standardize equations."
  (let (
        (ret "")
        (predpk (buffer-substring (point-min) (point-max)))
        (tvar (or the (esn-get-variable-names "THE")))
        (avar (or ome (esn-get-variable-names "OME")))
        (svar (or sig (esn-get-variable-names "SIG")))
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (case-fold-search 't)
        (tmp ""))
    (with-temp-buffer
      (set-syntax-table esn-mode-syntax-table)
      (insert predpk)
      ;; Change everything to THETA(x), ETA(x), and EPS(x)
      (esn-fix-numbering tvar avar svar)
      (goto-char (point-min))
      (setq tmp "")
      (while (re-search-forward (eval-when-compile (esn-reg-records)) nil t)
        (forward-char 1)
        (setq tmp (esn-rec3 (match-string 0)))
        (unless (string-match
                 (eval-when-compile
                   (esn-reg-record-exp
                    (regexp-opt '(
                                  "PK"
                                  "PRE"
                                  "ERR"
                                  "DES"
                                  "AES"
                                  "MIX"
                                  )
                                't
                                )
                    't)) tmp)
          (save-restriction
            (esn-narrow-rec)
            (delete-region (point-min) (point-max))
            (widen))))
      (goto-char (point-min))
      (while (re-search-forward ";.*" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "DEXP" nil t)
        (replace-match "EXP"))
      (goto-char (point-min))
      (while (re-search-forward (eval-when-compile (esn-reg-records 't)) nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+" nil t)
        (replace-match ""))

      ;; Shift all (ETA(1)+1) to (1+ETA(1))
      (goto-char (point-min))
      (while (re-search-forward "\\<(ETA(\\([0-9]+\\))\\+1)" nil t)
        (replace-match "(1+ETA(\\1))"))
      (unless do-theta
        ;; Shift ALL COV*THETA(x) -> THETA(x)*COV
        (goto-char (point-min))
        (while (re-search-forward "\\<\\([A-Z][A-Z0-9]*\\)\\*THETA(\\([0-9]+\\))" nil t)
          (replace-match "THETA(\\2)*\\1"))

        ;; Shift ALL (COV-#)*THETA(x) -> THETA(x)*(COV-#)
        (goto-char (point-min))
        (while (re-search-forward "(\\([A-Z][A-Z0-9]*-[0-9.]+\\))\\*THETA(\\([0-9]+\\))" nil t)
          (replace-match "THETA(\\2)*(\\1)"))

        ;; Shift all (THETA(x)*COV+1) -> (1+THETA(x)*COV)
        (goto-char (point-min))
        (while (re-search-forward "(THETA(\\([0-9]+\\))\\*\\([A-Z][A-Z0-9]*\\)+1)" nil t)
          (replace-match "(1+THETA(\\1)*\\2)"))

        ;; Shift all (COV)**THETA(y)*THETA(x) to THETA(x)*(COV)**THETA(y)
        (goto-char (point-min))
        (while (re-search-forward "\\([(]?[A-Z][A-Z0-9]*\\(?:/[0-9.]+\\)?[)]?\\*\\*[(]?THETA[(][0-9]+[)]\\{1,2\\}\\)\\*\\(THETA[(][0-9]+[)]\\)" nil t)
          (replace-match "\\2*\\1"))

        ;; Shift (THETA(x)*COV+1) -> (1+THETA(x)*COV)
        (goto-char (point-min))
        (while (re-search-forward "(\\(THETA([0-9]+)\\*[A-Z][A-Z0-9]*\\)\\+1)" nil t)
          (replace-match "(1+\\1)"))

        ;; Shift (1+THETA(x)*COV)*THETA(y) -> THETA(y)*(1+THETA(x)*COV)
        (goto-char (point-min))
        (while (re-search-forward "\\((1\\+THETA([0-9]+)\\*[A-Z][A-Z0-9]*)\\)\\*\\(THETA([0-9]+)\\)" nil t)
          (replace-match "\\2*\\1"))

        ;; Change proportional shifts
        ;; THETA(x)*(1+THETA(y)*COV) -> THETA(x)*(1~THETA(y)*COV)
        (goto-char (point-min))
        (while (re-search-forward "\\(THETA([0-9])\\*(1\\)\\+\\(THETA([0-9]+)\\*[A-Z][A-Z0-9]*)\\)" nil t)
          (replace-match "\\1~\\2")))
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil t)
        (replace-match "\n"))
      (setq ret (buffer-substring (point-min) (point-max))))
    (symbol-value 'ret)))

;;;###autoload
(defun esn-is-block-p ()
  "Determines if:
 1. We are in a OMEGA/SIGMA blocks
 2. We are in a BLOCK(#)"
  (if (not  (or (string= "OME" (esn-get-current-rec))
            (string= "SIG" (esn-get-current-rec)))) nil
    (save-match-data
      (save-excursion
        (save-restriction
          (esn-narrow-rec )
          (goto-char (point-min))
          (re-search-forward "\\<BLOCK\\(([0-9]+)\\)?" nil t))))))


;;;###autoload
(defun esn-in-comment-p (&optional msl)
  "Returns if the the point is in the comment or string.  Also
preserves match data."
  (save-match-data
    (let (
          (ret (memq (get-text-property (point) 'face) '(font-lock-string-face font-lock-doc-face))))
      (when (not ret)
        (setq ret (looking-back ";.*"))
        (unless ret
          (setq ret (looking-back (eval-when-compile (format "%s[ \t]+.*" (esn-reg-record-exp "PRO" t t)))))
          (unless ret
            (setq ret (looking-back "^[ \t]*\"[Cc*!].*"))
            (unless ret
              (setq ret (looking-back "^[ \t]*[Cc][ \t]+[^=\n].*"))
              (unless ret
                (setq ret (looking-back "^[ \t]*[*!].*"))))))
        (when msl
          (setq ret (or ret (save-excursion
                              (backward-char msl)
                              (esn-in-comment-p))))))
      (symbol-value 'ret))))

;;;###autoload
(defun esn-cancel-pending-unneeded-timers ()
  "* Cancels unneeded times when the current record changes"
  (when esn-align-equals-fun-timer
    (cancel-timer esn-align-equals-fun-timer))
  (when esn-align-matrix-timer
    (cancel-timer esn-align-matrix-timer))
  (when esn-table-split-count-timer
    (cancel-timer esn-table-split-count-timer)))

;;;###autoload
(defun esn-model-txt ()
  "* Returns a Text Rendition of the Model used"
  (cond
   ((esn-is-pred-p)
    "PRED")
   ('t
    (if (esn-advan)
        (format "ADVAN%s/TRANS%s" (esn-advan) (esn-trans))
      "Unknown"
      ))))

;;;###autoload
(defun esn-est-method-txt ()
  "* Returns a Text rendition of the Estimation method"
  (cond 
   ((esn-is-lap-p)
    "Laplacian"
    )
   ((esn-is-hybrid-p)
    "Hybrid"
    )
   ((esn-is-foi-posthoc-p)
    "FO+I+POSTHOC"
    )
   ((esn-is-fo-posthoc-p)
    "FO+POSTHOC"
    )
   ((esn-is-fo-p)
    "FO"
    )
   ((esn-is-foce-p)
    "FOCE"
    )
   ((esn-is-focei-p)
    "FOCE+I"
    )
   ((save-excursion
      (goto-char (point-min))
      (re-search-forward (eval-when-compile (esn-reg-record-exp "SIM")) nil t))
    "Simulation"
    )
   ((save-excursion
      (goto-char (point-min))
      (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t))
    "Estimation"
    )
   ('t
    "Unknown")))

;;;###autoload
(defun esn-is-foce-p ()
  "* Returns if the current control stream uses FOCE estimation (but NOT FOCEI)"
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let (
            (foce nil)
            (case-fold-search 't))
        (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t)) nil
          (save-restriction
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq foce (re-search-forward "\\<METH[A-Z]* *[= \t] *\\(1\\|COND[A-Z]*\\)\\(?:[ \n]\\|$\\)" nil t))
            (goto-char (point-min))
            (when (and foce (re-search-forward "\\<\\(?:INT\\|LAP\\)[A-Z]*" nil 't))
              (setq foce nil) ; Not interaction nor laplacian.
              )
            (widen)))
        (symbol-value 'foce)))))

;;;###autoload
(defun esn-is-lap-p ()
  "* Returns if the current control stream uses Laplacian estimation"
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let (
            (foce nil)
            (case-fold-search 't))
        (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t)) nil
          (save-restriction
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq foce (re-search-forward "\\<METH[A-Z]* *[= \t] *\\(1\\|COND[A-Z]*\\)[ \n]" nil t))
            (goto-char (point-min))
            (when (and foce (not (re-search-forward "\\<LAP[A-Z]*" nil 't))) 
              (setq foce nil) ; Laplacian not found.
              )
            (widen)))
        (symbol-value 'foce)))))

;;;###autoload
(defun esn-is-focei-p ()
  "* Returns if the current control stream uses FOCEI estimation"
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let (
            (foce nil)
            (case-fold-search 't))
        (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t)) nil
          (save-restriction
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq foce (re-search-forward "\\<METH[A-Z]* *[= \t] *\\(1\\|COND[A-Z]*\\)[ \n]" nil t))
            (goto-char (point-min))
            (when (and foce (not (re-search-forward "\\<INT[A-Z]*" nil 't)))
              (setq foce nil) ; Interaction not found.  set to nil.
              )
            (widen)))
        (symbol-value 'foce)))))

;;;###autoload
(defun esn-is-hybrid-p ()
  "* Returns if the current control stream uses Hybrid estimation."
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let (
            (foce nil)
            (case-fold-search 't))
        (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t)) nil
          (save-restriction
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq foce (re-search-forward "\\<METH[A-Z]* *[= \t] *\\(HYBR[A-Z]*\\)[ \n]" nil t))
            (widen)))
        (symbol-value 'foce)))))

;;;###autoload
(defun esn-is-fo-p ()
  "* Returns if the current control stream is FO."
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let (
            (fo nil)
            (case-fold-search 't))
        (when (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t)
          (save-restriction
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq fo (not (re-search-forward "\\<METH[A-Z]* *[= \t] *" nil t)))
            (unless fo
              (goto-char (point-min))
              (setq fo (re-search-forward "\\<METH[A-Z]* *[= \t] *\\(0\\|ZER[A-Z]*\\)[ \n]" nil t)))
            (widen)))
        (symbol-value 'fo)))))

;;;###autoload
(defun esn-is-fo-posthoc-p ()
  " * Returns if the current control stream is FO w/POSTHOC step (and NO interaction step)"
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let (
            (fo nil)
            (case-fold-search 't))
        (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t)) nil
          (save-restriction
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq fo (re-search-forward "\\<METH[A-Z]* *[= \t] *\\(0\\|ZER[A-Z]*\\)[ \n]" nil t))
            (goto-char (point-min))
            (unless (and fo (re-search-forward "\\<METH[A-Z]*" nil t))
              (setq fo 't) ;; No method found, Therefore, fo.
              )
            (goto-char (point-min))
            (when fo
              (unless (re-search-forward "\\<POS[A-Z]*" nil 't)
                (setq fo nil) ;; Posthoc not found, return nothing.
                ))
            (goto-char (point-min))
            (when (and fo (re-search-forward "\\<INT[A-Z]*" nil 't))
              (setq fo nil))
            (widen)))
        (symbol-value 'fo)))))

;;;###autoload
(defun esn-is-foi-p ()
  " * Returns if the current control stream is FOI w/POSTHOC step"
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let (
            (fo nil)
            (case-fold-search 't))
        (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t)) nil 
          (save-restriction
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq fo (re-search-forward "\\<METH[A-Z]* *[= \t] *\\(0\\|ZER[A-Z]*\\)[ \n]" nil t))
            (goto-char (point-min))
            (unless (and fo (re-search-forward "\\<METH[A-Z]*" nil t))
              (setq fo 't) ;; No method found, Therefore, fo.
              )
            (goto-char (point-min))
            (when (and fo (not (re-search-forward "\\<INT[A-Z]*" nil 't)))
              (setq fo nil) ; No interaction step, set to nil.
              )
            (widen)))
        (symbol-value 'fo)))))
;;;###autoload
(defun esn-is-foi-posthoc-p ()
  " * Returns if the current control stream is FOI w/POSTHOC step"
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let (
            (fo nil)
            (case-fold-search 't))
        (if (not (re-search-forward (eval-when-compile (esn-reg-record-exp "EST")) nil t)) nil 
          (save-restriction
            (end-of-line)
            (esn-narrow-rec)
            (goto-char (point-min))
            (setq fo (re-search-forward "\\<METH[A-Z]* *[= \t] *\\(0\\|ZER[A-Z]*\\)[ \n]" nil t))
            (goto-char (point-min))
            (unless (and fo (re-search-forward "\\<METH[A-Z]*" nil t))
              (setq fo 't) ;; No method found, Therefore, fo.
              )
            (goto-char (point-min))
            (when fo
              (unless (re-search-forward "\\<POS[A-Z]*" nil 't)
                (setq fo nil) ;; Posthoc not found, return nothing.
                ))
            (goto-char (point-min))
            (when (and fo (not (re-search-forward "\\<INT[A-Z]*" nil 't)))
              (setq fo nil) ; No interaction step, set to nil.
              )
            (widen)))
        (symbol-value 'fo)))))
(defvar esn-rec-cache '()
  "*Esn Rec cache")
(make-variable-buffer-local 'esn-rec-cache)

;;;###autoload
(defun esn-rec (rec &optional cont probOne destructive)
  "* This returns if a record is present. If cont is true, return its contents.
If prob-one, only the first problem is considered.
If destructive is true, remove the records after obtaining them.
"
  (save-restriction
    (let (
          (case-fold-search 't)
          (ret (if cont
                   ""
                 nil)))
      (save-excursion
        (when probOne (esn-narrow-to-problem 1))
        (goto-char (point-min))
        (if (re-search-forward (esn-reg-record-exp rec nil 't) nil t)
            (if (not cont)
                (setq ret (point))
              (save-restriction
                (esn-narrow-rec)
                (setq ret (buffer-substring (point-min) (point-max)))
                (when destructive
                  (delete-region (point-min) (point-max))
                  (when (eq (type-of destructive) 'string)
                    (insert destructive)))
                (goto-char (point-max)))))
        (if (not cont) nil
          (if (not ret) nil
                                        ; (when probOne (esn-narrow-to-problem 1))
            (while (re-search-forward (esn-reg-record-exp rec nil 't) nil t)
              (save-restriction
                (esn-narrow-rec)
                (setq ret (concat ret "\n" (buffer-substring (point-min) (point-max))))
                (when destructive
                  (delete-region (point-min) (point-max))
                  (when (eq (type-of destructive) 'string)
                    (insert destructive)))
                (goto-char (point-max))
                (widen))))))
      (symbol-value 'ret))))

;;;###autoload
(defun esn-max-what (what)
  "Gets the maximum value of the variable what"
  (let (
        (mx 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (format "\\<%s(\\([0-9]+\\))" what) nil t)
        (setq mx (max mx (string-to-number (match-string 1))))))
    (symbol-value 'mx)))

;;;###autoload
(defun esn-max-eps (&optional no-extended)
  (let (
        (num 0)
        (check 't)
        (var (reverse (esn-get-variable-names "SIG"))))
    (unless no-extended
      (mapc (lambda (x)
              (when check
                (if (not (string= x ""))
                    (setq check nil)
                  (setq num (+ num 1)))))
            var
            ))
    (setq num (max (esn-max-what "\\(?:EPS\\|ERR\\)") (if no-extended 0 (- (length var) num))))
    (symbol-value 'num)))

;;;###autoload
(defun esn-max-eta (&optional no-extended)
  (let (
        (num 0)
        (check 't)
        (var (reverse (esn-get-variable-names "OME")))
        (lv 0))
    (unless no-extended
      (mapc (lambda (x)
              (when check
                (if (not (string= x ""))
                    (setq check nil)
                  (setq num (+ num 1)))))
            var
            ))
    (setq num (max (esn-max-what "ETA") (if no-extended 0 (- (length var) num))))
    (symbol-value 'num)))

;;;###autoload
(defun esn-max-theta (&optional no-extended)
  (let (
        (num 0)
        (check 't)
        (var (reverse (esn-get-variable-names "THE"))))
    (unless no-extended
      (mapc (lambda (x)
              (when check
                (if (not (string= x ""))
                    (setq check nil)
                  (setq num (+ num 1)))))
            var
            ))
    (setq num (max (esn-max-what "THETA") (if no-extended 0 (- (length var) num))))
    (symbol-value 'num)))
(defun esn-max-cden_ ()
  (esn-max-what "CDEN_"))
(defun esn-max-mtime ()
  (esn-max-what "MTIME"))
(defun esn-max-mnext ()
  (esn-max-what "MNEXT"))
(defun esn-max-mpast ()
  (esn-max-what "MPAST"))
(defun esn-max-a ()
  (esn-max-what "A"))
(defun esn-max-dadt()
  (esn-max-what "DADT"))
(defun esn-max-thsimp()
  (esn-max-what "THSIMP"))
(defun esn-max-omsimp()
  (esn-max-what "THSIMP"))

;;;###autoload
(defun esn-is-pred-p ()
  "Determines if the current control stream is a pred control stream.  Looks for the $PRED record."
  (let (
        (pred nil)
        (case-fold-search 't))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (eval-when-compile (esn-reg-record-exp "PRE" 't)) nil t)
          (setq pred 't)))
    (symbol-value 'pred)))

;;;###autoload
(defun esn-num-problems ()
  "Returns number of problems in control stream."
  (let (
        (multi 0))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward (eval-when-compile (esn-reg-record-exp "PRO" 't)) nil t)
        (end-of-line)
        (setq multi 1)
        (while (re-search-forward (eval-when-compile (esn-reg-record-exp "PRO" 't)) nil t)
          (setq multi (+ multi 1)))))
    (symbol-value 'multi)))
(defun esn-mode-is-log (&optional force-check check-exp)
  "* Defines if DV is on a log scale by:
 (1) Checking to see if file name contains \"-log-\"
 (2) Check to see if Purpose statement contains \"Log Error\" or \"Lognormal Error\"
 (3) Checking against the  variables inputted (lines  2-10 lines) to see if EXP(VAR)=DV for all appropriate values. If so return the variable aliases assigned to the log-transformed DV. Also assign esn-mode-current-dv to the current dv value.

If force-check is enabled, only use #3 to determine if esn-mode-is-log.

If check-exp is enabled change the esn-mode-is-log to esn-mode-is-regular

"
  
  (interactive)
  (let (
        (case-fold-search 't)
        (data "")
        (inp (esn-rec "INP" 't 't))
        (dv -1)
        (expn -1)
        (lgdv -1)
        (inform "Cannot determine if this is log-transfored data, assume not normal-scale.")
        (is-log nil)
        (n-dat 0)
        (pt nil)
        (fn (buffer-file-name))
        i)
    (if (and (not force-check) (progn
                                 (let ((ret (and fn (string-match "-log-" fn))))
                                   (if check-exp
                                       (not ret)
                                     ret))))
        (setq is-log 't)
      (if (and (not force-check) (progn
                                   (let ((ret (string-match "\\<Log\\(normal\\)?[ \t]Error\\>" (esn-update-get-purpose 't))))
                                     (if check-exp
                                         (not ret)
                                       ret))))
          (setq is-log 't)
        (with-temp-buffer
          (insert inp)
          (goto-char (point-min))
          (while (re-search-forward (eval-when-compile (esn-reg-record-exp "INP" 't)) nil t)
            (replace-match ""))
          (goto-char (point-min))
          (while (re-search-forward ";.*" nil t)
            (replace-match ""))
          (goto-char (point-min))
          (while (re-search-forward "\n" nil t)
            (replace-match ""))
          (setq inp (buffer-substring (point-min) (point-max)))
          (goto-char (point-min))
          (setq esn-mode-current-dv nil)
          (when (re-search-forward "\\(?:\\<\\|=\\)DV\\(?:\\>\\|=\\|$\\)" nil t)
            (re-search-forward "\\= *= *[^ ]" nil t)
            (setq pt (point))
            (while (not (bobp))
              (esn-backward-w)
              (unless esn-mode-current-dv
                (setq esn-mode-current-dv (buffer-substring (point) pt)))
              (skip-chars-backward "[ \t\n]")
              (setq dv (+ dv 1)))))
        (setq pt nil)
        (save-excursion
          (esn-narrow-to-current-problem)
          (goto-char (point-min))
          (if (not (re-search-forward (eval-when-compile (esn-reg-data-rec "DAT")) nil t))
              (message inform)
            (setq data (match-string 2))
            (if (string-match (format "^%s" (regexp-quote esn-completing-current-directory)) data)
                (setq data (replace-match "" nil nil data)))
            (goto-char (point-min))
            (unless (string=  "" data)
              (let (
                    (first-line (if esn-w32
                                    (if (file-exists-p (concat esn-path "bin/head.exe"))
                                        (concat (concat esn-path "bin/head.exe " data))
                                      (concat "head " data))
                                  (concat "head " data))))
                (setq first-line (split-string (esn-command-to-string first-line) "\n"))
                (pop first-line)

                (mapc (lambda(line)
                        (let (
                              (cur-line (split-string line "\\([,\t]+\\|  +\\)"))
                              (cdv ""))
                          (setq n-dat (max n-dat (length cur-line)))
                          (setq cdv (nth dv cur-line))
                          (when (and cdv
                                     (string-match "[0-9]" cdv)
                                     (string-match "^[0-9.E+-]+$" cdv))
                            (setq cdv (string-to-number cdv))
                            (when check-exp
                              (setq cdv (log cdv)))
                            (when (= expn -1)
                              ;; Now get the Log number if not present.
                              (mapc (lambda (num)
                                      (let (
                                            (anum -1))
                                        (setq lgdv (+ lgdv 1))
                                        (when (and num
                                                   (string-match "[0-9]" num)
                                                   (string-match "^[0-9.E+-]+$" num))

                                          (setq anum (string-to-number num))
                                          (when (and (if check-exp
                                                         't
                                                       (< 0 anum))
                                                     (> esn-mode-log-equal (abs (- (if check-exp
                                                                                       anum
                                                                                     (log anum))
                                                                                   cdv))))
                                            (setq expn lgdv)
                                            (setq is-log 't)))))
                                    cur-line))
                            (when (and (not (= expn -1)) is-log)
                              ;;
                              (let ( (num (nth expn cur-line)))
                                (if (not (and num
                                              (string-match "[0-9]" num)
                                              (string-match "^[0-9.E+-]+$" num)))
                                    (setq is-log nil)
                                  (setq num (string-to-number num))
                                  (if (and (not check-exp) (>= 0 num))
                                      (setq is-log nil)
                                    (unless (> esn-mode-log-equal (abs (- (if check-exp
                                                                              num
                                                                            (log num))
                                                                          cdv)))
                                      (setq is-log nil)))))))))
                      first-line)))))
        (when is-log
          (setq is-log (- n-dat expn))
          (with-temp-buffer
            (insert inp)
            (goto-char (point-min))
            (when (re-search-forward (eval-when-compile (esn-reg-record-exp "INP" 't 't)) nil t)
              (replace-match " "))
            (goto-char (point-max))
            (setq i 0)
            (while (not (= (point) (point-min)))
              (esn-backward-w)
              (skip-chars-backward "\t \n")
              (setq i (+ i 1)))
            (cond
             ( (> i n-dat)
               (setq is-log (+ is-log (- i n-dat))))
             ( (< i n-dat)
               (setq is-log (- is-log (- n-dat i))))
             ( (= i n-dat)))
            (goto-char (point-max))
            (setq n-dat 1)
            (setq pt (point))
            (while (> is-log n-dat)
              (esn-backward-w)
              (setq n-dat (+ n-dat 1))
              (setq pt (point)))
            (esn-backward-w)
            (setq is-log (buffer-substring (point) pt))
            (when (string-match " *=? *\\(DROP\\|SKIP\\)" is-log)
              (setq is-log (replace-match "" nil nil is-log)))
            (while (string-match " +" is-log)
              (setq is-log (replace-match "" nil nil is-log)))
            (while (string-match " +" esn-mode-current-dv)
              (setq esn-mode-current-dv (replace-match "" nil nil esn-mode-current-dv)))
            (setq esn-mode-current-dv (split-string  esn-mode-current-dv "="))
            (setq is-log (split-string is-log "="))))))
    (symbol-value 'is-log)))

(defalias 'e 'esn-get-fixed-thetas)
(defvar esn-get-fixed-thetas-cached nil
  "Are the fixed thetas cached?")
(defvar esn-get-fixed-thetas-lst nil
  "List of fixed thetas")
(make-variable-buffer-local 'esn-get-fixed-thetas-lst)
(make-variable-buffer-local 'esn-get-fixed-thetas-cached)
;; Clear cache on theta modification.
(esn-rec-modification-hook "theta" (lambda() (setq esn-get-fixed-thetas-cached nil)))
;;;###autoload
(defun esn-get-fixed-thetas ()
  "Gets the thetas that are fixed.  It will be cached until the user modifies the $THETA block."
  (if esn-get-fixed-thetas-cached
      esn-get-fixed-thetas-lst
    (let ((theta (esn-rec "THE" t nil nil))
	  (start 0)
	  (fixed-thetas '())
	  (i 0))
      (while (string-match ";.*" theta start)
	(setq theta (replace-match "" t t theta))
	(setq start (match-beginning 0)))
      (setq start 0)
      (while (string-match "\\(?:([^)]*)\\|[0-9.]+\\)\\([ \t]?FIX\\(?:ED?\\)?\\)?" theta start)
	(setq i (+ i 1))
	(when (save-match-data (string-match "\\<FIX\\(?:ED?\\)?\\>" (match-string 0 theta)))
	  (add-to-list 'fixed-thetas i))
	(setq start (match-end 0)))
      (setq esn-get-fixed-thetas-cached t)
      (setq esn-get-fixed-thetas-lst fixed-thetas)
      fixed-thetas)))

(provide 'esn-properties)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-properties.el ends here
