;;; esn-fun.el --- General Emacs Speaks NONMEM functions
;;
;; Filename: esn-fun.el
;; Description:
;; Author: Matthew L. Fidler
;; Maintainer:
;; Created: Thu Mar 11 16:43:46 2010 (-0600)
;; Version:
;; Last-Updated:
;;           By:
;;     Update #: 227
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
;; 17-Jan-2011    Matthew L. Fidler  
;;    Started up and down toggle keys.
;; 06-Oct-2010    Matthew L. Fidler
;;    Changed esn-insert-after to allow snippet insertion
;; 06-Oct-2010    Matthew L. Fidler
;;    Changed esn-insert-after to allow a blank insertion to just goto that point.
;; 22-Sep-2010    Matthew L. Fidler
;;    Added esn-format that takes %1% and %2% expressions
;; 03-Sep-2010    Matthew L. Fidler
;;    Made sure that esn-rec3 returns a value that is ALWAYS upper case.
;; 30-Aug-2010    Matthew L. Fidler
;;    Added esn-is-between and esn-add-to-alist
;; 19-Aug-2010    Matthew L. Fidler
;;    Tried to standardize record regular expressions.
;; 19-Aug-2010    Matthew L. Fidler
;;    Moved Esn-rec3 to this file.
;; 19-Aug-2010    Matthew L. Fidler
;;    Moved esn-narrow-to-problem to esn-narrow.
;; 17-Jun-2010    Matthew L. Fidler
;;    Added saving file information (per file) in the esn-abs-dir-hash
;; 05-May-2010    Matthew L. Fidler
;;    Added esn-file-exists
;; 19-Apr-2010    Matthew L. Fidler
;;    Fixed renumbering bug that looked in comments for values.
;; 19-Apr-2010    Matthew L. Fidler
;;    Changed renumbering to only fix extended control streams when translation
;;    is enabled.
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
(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-in-comment-p "esn-properties")
(declare-function esn-number-est "esn-align")
(declare-function esn-is-block-p "esn-properties")
(declare-function esn-max-theta "esn-properties")
(declare-function esn-max-eta "esn-properties")
(declare-function esn-max-eps "esn-properties")
(declare-function esn-fix-numbering "esn-extended")
(declare-function esn-undo-numbering "esn-extended")
(declare-function esn-max-what "esn-properties")
(declare-function esn-get-current-record "esn-narrow")


(defun esn-get-data ()
  "Gets the data for the current file/problem."
  (save-restriction
    (save-excursion
      (let ((case-fold-search 't)
            (data "")
            (inhibit-read-only 't))
        (esn-narrow-to-current-problem)
        (goto-char (point-min))
        (if (not (re-search-forward
                  (eval-when-compile (esn-reg-data-rec "DAT")) nil t)) nil
          (setq data (match-string 2))
          (if (string-match (format "^%s" (regexp-quote esn-completing-current-directory)) data)
              (setq data (replace-match "" nil nil data))))
        (symbol-value 'data)))))

(defun esn-add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let (
        (case-fold-search 't)
        (existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var))))))


(defun esn-is-between (first last &optional eof)
  "Returns true of the carat is between the first and last .

If eof is true, then the last position is:
   (1) the position of the variable last
   (2) the position of the next variable first
   (3) the end of the buffer.
If eof is nil, then the last position is:
   (1) the position of the next variable last.
"
  (let (
        (case-fold-search 't)
        (first-posB nil)
        (last-posF nil)
        (between 't))
    (save-excursion
      (if (re-search-backward first nil t)
          (setq first-posB (point))
        (setq between nil)))
    (if between
        (save-excursion
          (if (re-search-forward last nil t)
              (setq last-posF (point))
            ;;
            ;; Last not found, look for first.
            ;;
            (if eof
                (progn
                  (if (re-search-forward first nil t)
                      (setq last-posF (point))
                    ;;
                    ;; First not found, set to end of buffer.
                    ;;
                    (setq last-posF (point-max))))
              ;;
              ;; Eof not true.
              ;;
              (setq between nil)))))
    (if (and (not eof) between)
        (save-excursion
          (if (re-search-forward first nil t)
              (if (< (point) last-posF)
                  (setq between nil)))))
    (if between
        (save-excursion
          (if (re-search-backward last nil t)
              (if (> (point) first-posB)
                  (setq between nil)))))
    (symbol-value 'between)))

;; Cache esn-rec3
(defvar esn-rec3-assoc '()
  "* Associative array for records"
  )
(defvar esn-rec3-assoc-eq '()
  "* Associative array for records"
  )


;; This is called so often it needs to be cached.
;;;###autoload
(defun esn-rec3 (string &optional no-equals)
  "Changes a string to a current record/option abbreviation

Current Record = Three letters of current record excluding $

For $AES0 and $AESINITIAL return AES0

When OPTION=, return OPT=
"
  (if (assoc string (if no-equals esn-rec3-assoc esn-rec3-assoc-eq))
      (cadr (assoc string (if no-equals esn-rec3-assoc esn-rec3-assoc-eq)))
    (let (
          (rec (upcase string))
          eq
          )
      (if (not string)
          nil
        (save-match-data
          (setq eq (string-match "=$" rec))
          (when eq
            (setq rec (replace-match "" 't 't rec)))
          (when (string-match "^[$]" rec)
            (setq rec (replace-match "" nil nil rec)))
          (when (> (length rec) 4)
            (setq rec (substring rec 0 4)))
          (if (or (string= "AES0" rec)
                  (string= "AESI" rec))
              (setq rec "AES0")
            (if (string-match "^INFI" rec) ; $INFILE and $INFN have the same 3-character code.
                (setq rec "DAT")
              (when (> (length rec) 3)
                (setq rec (substring rec 0 3))))))
        (when (and eq (not no-equals))
          (setq rec (concat rec "=")))
        (if no-equals
            (esn-add-to-alist 'esn-rec3-assoc (list string rec))
          (esn-add-to-alist 'esn-rec3-assoc-eq (list string rec)))
        (symbol-value 'rec)))))

;;;###autoload
(defun esn-infusion (&optional reg)
  "Determines if there is any infusion information in the model."
  (let (
        (case-fold-search 't)
        (reg (or reg "\\<RATE\\>"))
        (inp (esn-rec "INP" 't))
        (ret nil))
    (while (string-match ";.*" inp)
      (replace-match ""))
    (when (string-match reg inp)
      (setq ret 't))
    (symbol-value 'ret)))

;;;###autoload
(defun esn-trans ()
  "Gets the TRANS number of the run."
  (let (
        (advan nil)
        (case-fold-search 't))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\<TRANS\\([0-9]+\\)" nil t)
        (setq advan (string-to-number (match-string 1)))))
    (symbol-value 'advan)))

;;;###autoload
(defun esn-advan ()
  "Gets the ADVAN number of the run."
  (let (
        (advan nil)
        (case-fold-search 't))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "\\<ADVAN\\([0-9]+\\)" nil t)
        (setq advan (string-to-number (match-string 1)))))
    (symbol-value 'advan)))

;;;###autoload
(defun esn-user-full-name (&optional uid)
  "Gets the full user name or prompts for it and saves to .emacs if not present."
  (interactive)
  (if (or (not uid) (and uid (string= (user-login-name uid) (user-login-name))))
      (let (
            (name (user-full-name)))
        (when (or (not name) (string= "unknown" name) (string-match "^[ \t\n]*$" name))
          (setq name user-full-name)
          (when (or (not name) (string= "unknown" name) (string-match "^[ \t\n]*$" name))
            (setq name (esn-prompt "Your Full Name: "
                                   "^[ \t]*[A-Za-z]* [A-Za-z0-9. ]*[ \t]*$"
                                   "You must input at least your first and last name: "))
            (when (string-match "^[ \t]*" name)
              (setq name (replace-match "" nil nil name)))
            (when (string-match "[ \t]]*$" name)
              (setq name (replace-match "" nil nil name)))
            (setq user-full-name name)
            (customize-save-variable 'user-full-name user-full-name)
            (customize-save-customized)))
        (symbol-value 'name))
    "Unknown"))

;;;###autoload
(defun esn-narrow-to-problem (&optional nm)
  "Narrow to the problem number num."
  (interactive)
  (let (
        (case-fold-search 't)
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (narrowed nil)
        (i 0)
        (begin nil)
        (end nil)
        (num (or nm 1)))
    (save-excursion
      (goto-char (point-min))
      (while (< i num)
        (re-search-forward "\\<\\$PRO" nil t)
        (setq i (+ i 1)))
      (setq begin (point))
      (if (re-search-forward "\\<\\$PRO" nil t)
          (progn
            (beginning-of-line)
            (setq end (point))
            (goto-char begin)
            (beginning-of-line)
            (setq begin (point))
            (narrow-to-region begin end)
            (setq narrowed 't))
        (goto-char begin)
        (beginning-of-line)
        (narrow-to-region (point) (point-max))
        (setq narrowed 't)))
    (symbol-value 'narrowed)))

;;;###autoload
(defun esn-in-rec-p ()
  "* Defines point is in record or not"
  (save-restriction
    (esn-narrow-rec)))

;;
;; This defines the filling/wrapping functions
;;
;;;###autoload
(defun esn-forward-w (&optional chars)
  "Defines the forward-word routine used for wrapping.  See `esn-backward-w'."
  (interactive)
  (skip-chars-forward (or chars " "))
  (let (
        (case-fold-search 't)
        (ret "")
        (point1 (point))
        (point2 (point))
        )
    (if (re-search-forward "\\=\\(\\w+ *= *\\w+\\)" nil t) (setq ret (match-string 1))
      (if (re-search-forward "\\=\\(([^)\n]+) +FIXED\\)" nil t) (setq ret (match-string 1))
        (if (re-search-forward "\\=\\(([^)\n]+)\\)" nil t) (setq ret (match-string 1))
          (if (re-search-forward "\\=\\(\\w+ *= *([^)\n]+)\\)" nil t) (setq ret (match-string 1))
            (if (re-search-forward "\\=\\(([^)\n]+) *= *([^)\n]+)\\)" nil t) (setq ret (match-string 1))
              (if (re-search-forward "\\=\\(([^)\n]+) *= *\\w+ \\)" nil t) (setq ret (match-string 1))
                (if (re-search-forward "\\=\\(\\w+ +FIXED\\)" nil t) (setq ret (match-string 1))
                  (if (re-search-forward "\\=\\(\\w+(\\w+)\\)" nil t) (setq ret (match-string 1))
                    (setq point1 (point))
                    (skip-chars-forward " \t\n")
                    (re-search-forward "\\=[^ \t\n]*")
                    (setq point2 (point))
                    (setq ret (buffer-substring point1 point2))))))))))
    (if (string-match "^[ \t\n]+" ret)
        (setq ret (replace-match "" nil nil ret)))
    (symbol-value 'ret)))

;;;###autoload
(defun esn-backward-w (&optional chars)
  "Defines the backward-word routine used for wrapping.
Hence these are things that should not be separated on a line.
 (1) A = B is a word
 (2) (A B C) is a word
 (3) A = (B C) is a word.
 (4) 0.9 FIXED is a word
 (5) (0 1 2) FIXED is a word (NM6)"
  (interactive)
  (skip-chars-backward (or chars " "))
  (if (re-search-backward "[ \n][^ \n]*\\=" nil t)
      (progn
        (forward-char 1)))
  (if (looking-at "FIXED")
      (esn-backward-w)
    (if (not (looking-at "[^\n(]*)"))
        (re-search-backward "([^)\n]*?\\=" nil t)
      (re-search-backward "[(]" nil t))
    (if (not (looking-at " *= *[^ \n]"))
        (if (re-search-backward "[ \n][^ \n]+ *= *\\=" nil t)
            (forward-char 1))
      (if (re-search-backward "[ \n][^ \n]+[ \n]*\\=" nil t)
          (forward-char 1)))))

;;;###autoload
(defun esn-search-in-record (re rec)
  "* Searches re within the record rec."
  (save-restriction
    (let (
          (case-fold-search 't)
          (ret nil))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward (format "^$%s" rec) nil t)
        (save-excursion
          (beginning-of-line)
          (skip-chars-backward "\n \t")
          (if (looking-at "[ \t\n]+")
              (replace-match "\n\n")))
        (end-of-line)
        (skip-chars-forward " \n\t")
        (esn-narrow-rec)
        (goto-char (point-min))
        (setq ret (re-search-forward re nil t))
        (widen))
      (symbol-value 'ret))))

;;;###autoload
(defun esn-replace-in-record (re rep rec &optional all remEmpty)
  "* Replaces re with rep in the record rec.  If not found return nil, otherwise return true.
all = replace all matches
remEmpty = delete empty record.
"
  (save-restriction
    (let (
          (case-fold-search 't)
          (sub 't))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward (format "^$%s" rec) nil t)
        (save-excursion
          (beginning-of-line)
          (skip-chars-backward "\n \t")
          (if (looking-at "[ \t\n]+")
              (replace-match "\n\n")))
        (end-of-line)
        (esn-narrow-rec)
        (goto-char (point-min))
        (if (not all)
            (if (re-search-forward re nil t)
                (replace-match rep)
              (setq sub nil))
          (setq sub nil)
          (while (re-search-forward re nil t)
            (replace-match rep)
            (setq sub 't)))
        (if (not remEmpty) nil
          (goto-char (point-min))
          (if (not (re-search-forward (eval-when-compile (esn-reg-records nil 't)) nil t)) nil
            (if (= (point) (point-max))
                (delete-region (point-min) (point-max)))))
        (goto-char (point-max))
        (widen)
        (skip-chars-backward " \n\t")
        (if (looking-at "[ \n\t]+")
            (replace-match "\n\n")))
      (symbol-value 'sub))))
;; This file fixes the nonmem theta/eta/sigma parameters.
;;

;;;###autoload
(defun esn-insert-after (what lst &optional gotopoint is-yas-snippet)
  "Inserts a text what after records lst"
  (let (
        (case-fold-search 't)
        (pt nil)
        (pt2 nil)
        snip
        p0
        )

    (save-excursion
      (setq p0 (point))
      (if (eq esn-lastlst lst)
          (goto-char esn-lastpoint)
        (goto-char (point-max))
        (goto-char (point-min))
        (while (re-search-forward
                (esn-reg-record-exp lst) nil t)
          (end-of-line)
          (skip-chars-forward " \n\t"))
        (if (not  (eobp)) nil
          (insert "\n"))
        (if (not (re-search-forward (eval-when-compile
                                      (esn-reg-records nil 't)) nil t))
            (progn
              (goto-char (point-max))
              ;; For localwords spell checking.
              (if (search-backward "Localwords: " nil t)
                  (progn
                    (forward-line -1)
                    (end-of-line))))
          (backward-char (length (match-string 0))))
        (setq pt2 (point))
        (forward-line -1)
        (beginning-of-line)
        (if (not (looking-at ";| +Variables +[0-9]+/20 +|;"))
            (goto-char pt2)
          (setq pt2 (point)))
        (save-excursion
          (if (not (re-search-backward "[^\n]" nil t)) nil
            (forward-char 1)
            (if (looking-at "[ \t\n]+")
                (replace-match "\n"))))
        (unless (string= "" what)
          (if (string= (substring what 0 1) "\n")
              (setq what (substring what 1)))))
      (unless (string= "" what)
        (if (not is-yas-snippet)
            (insert what)
          (cond
           ( (fboundp 'yas/expand-snippet)
             (setq snip 't))
           ( 't
             (insert what)
             (when (re-search-backward (regexp-quote "$1") nil 't)
               (replace-match ""))))))
      (setq esn-lastpoint (point))
      (setq pt2 (point))
      (skip-chars-backward " \t\n")
      (if (looking-at "[ \t\n]+")
          (replace-match "\n"))
      (goto-char pt2)
      (skip-chars-backward " \t\n")
      (if (looking-at "[ \t\n]+")
          (replace-match "\n"))
      (setq pt (point)))
    (when (or snip  gotopoint)
      (goto-char pt))
    (when snip
      (yas/expand-snippet what)))
  nil)

;;;###autoload
(defun esn-renumber-com ()
  "Renumber COM.  Changes COMSAV if appropriate."
  (interactive)
  (let* (
         (inhibit-read-only 't)
         (inhibit-point-motion-hooks 't)
         (case-fold-search 't)
         (search (esn-search-in-record "\\<COMSAV *= *\\([0-9]+\\)" "ABB"))
         (comsav (if search
                     (string-to-number (match-string 1))
                   0
                   ))
         (lst (esn-renumber "COM" comsav))
         (ret (nth 0 lst))
         (ncomsav (- comsav (nth 1 lst))))
    (if (= ret 0)
        (esn-replace-in-record "\\<\\(COMSAV\\|COMRES\\) *= *[0-9]+"
                               "" "ABB" 't 't)
      (esn-replace-in-record "\\<COMSAV *= *[0-9]+"
                             (if (= comsav 0)
                                 ""
                               (format "COMSAV=%s" ncomsav))
                             "ABB"
                             )
      (esn-replace-in-record "\\<COMRES *= *[0-9]+"
                             (format "COMRES=%s" ret)
                             "ABB"
                             ))
    (symbol-value 'ret)))

;;;###autoload

(defun esn-eta-up ()
  "Defines alt-up swapping in $OMEGA block"
  (interactive)
  (esn-theta-up "OME"))

(defun esn-eta-down ()
  "Defines alt-down swapping in $OMEGA block"
  (interactive)
  (esn-theta-down "OME"))
(defalias 'e 'esn-estimate-up)
(defun esn-last-theta-est ()
  "Returns the match of the last estimate."
  (let (in-comment
        ret1
        ret2
        md1
        md2)
    (save-match-data
      ;; Find (0 x 2) FIXEd type estimates.
      (save-excursion
        (setq ret1 (re-search-backward "([^)])\\([ \t]+FIX\\(?:ED?\\)\\)?" nil t))
        (while (and (progn
                      (setq in-comment (esn-in-comment-p))
                      (symbol-value 'in-comment))
                    (progn
                      (setq ret1 (re-search-backward "([^)])\\([ \t]+FIX\\(?:ED?\\)\\)?" nil t))
                      (symbol-value 'ret1))))
        (setq md1 (set-match-data)))
      ;; FIND ### FIXED type estimates
      (save-excursion
        (setq ret2 (re-search-backward "[0-9.]+\\([ \t]+FIX\\(?:ED?\\)\\)?" nil t))
        (while (and (progn
                      (setq in-comment (esn-in-comment-p))
                      (symbol-value 'in-comment))
                    (progn
                      (setq ret2 (re-search-backward "[0-9.]+\\([ \t]+FIX\\(?:ED?\\)?\\)?" nil t))
                      (symbol-value 'ret2))))
        (setq md2 (set-match-data)))
      ;; Now check to see which was the last estimate
      )))

;; TODO need to handle THETA(2) or cases where the previous line is a $THETA definition.
(defun esn-theta-up (&optional what)
  "Defines the alt-up swapping in $theta block"
  (interactive)
  (esn-number-est what) ;; Force numbering of estimates
  (let ((line (buffer-substring (point-at-bol) (point-at-eol)))
        (theta (cond
                ((not what) "THETA")
                ((string= what "OME") "ETA")
                ((string= what "SIG") "EPS")))
        theta1 theta2)
    (if (not (or (string= "THETA" theta) (not (esn-is-block-p))))
        (error "Have to be outside of a BLOCK.")
      (if  (not (string-match (concat "\\<" theta "(\\([0-9]+\\))") line))
          (error "THETA(#) must be specified in comment line")
        (setq theta1 (match-string 1 line))
        (if (string= "1" theta1)
            (error "Cannot Change %s(1) to %s(0)..." theta theta)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (format "\\<%s(%s)" theta theta1) nil t)
              (replace-match (concat theta "(#######)") t t))
            (setq theta1 (- (string-to-number theta1) 1))
            (goto-char (point-min))
            (while (re-search-forward (format "\\<%s(%s)" theta theta1) nil t)
              (replace-match (format "%s(%s)" theta (+ 1 theta1))))
            (goto-char (point-min))
            (while (re-search-forward (concat "\\<" theta "(#######)") nil t)
              (replace-match (format "%s(%s)" theta theta1))))
          (save-excursion
            (forward-line -1)
            (setq line (buffer-substring (- (point-at-bol) 1) (+ 0 (point-at-eol))))
            (delete-region (- (point-at-bol) 1) (+ 0 (point-at-eol))))
          (save-excursion
            (goto-char (point-at-eol))
            (insert line))
          (esn-number-est what))))))

                                        ; Need to handle THETA(1) or points where $THETA is defined.
;;;###autoload
(defun esn-theta-down (&optional what)
  "Defines the alt-down swapping in $theta block"
  (interactive)
  (esn-number-est what) ;; Force numbering of estimates
  (let ((line (buffer-substring (point-at-bol) (point-at-eol)))
        (theta (cond
                ((not what) "THETA")
                ((string= what "OME") "ETA")
                ((string= what "SIG") "EPS")))
        (mx (cond
             ((not what) (esn-max-theta))
             ((string= what "OME") (esn-max-eta))
             ((string= what "SIG") (esn-max-eps))))
        theta1 theta2)
    (if (not (or (string= "THETA" theta) (not (esn-is-block-p))))
        (error "Have to be outside of a BLOCK.")
      (if  (not (string-match (concat "\\<" theta "(\\([0-9]+\\))") line))
          (error "%s(#) must be specified in comment line" theta)
        (setq theta1 (match-string 1 line))
        (if (string= (format "%s" mx) theta1)
            (error "Cannot Change past the maximum %s(%s)..." theta mx)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward (format "\\<%s(%s)" theta theta1) nil t)
              (replace-match (concat theta "(#######)") t t))
            (setq theta1 (string-to-number theta1))
            (goto-char (point-min))
            (while (re-search-forward (format "\\<%s(%s)" theta (+ 1 theta1)) nil t)
              (replace-match (format "%s(%s)" theta theta1)))
            (goto-char (point-min))
            (while (re-search-forward (concat "\\<" theta "(#######)") nil t)
              (replace-match (format "%s(%s)" theta (+ 1 theta1)))))
          (save-excursion
            (forward-line 1)
            (setq line (buffer-substring (- (point-at-bol) 0) (+ 1 (point-at-eol))))
            (delete-region (- (point-at-bol) 0) (+ 1 (point-at-eol))))
          (save-excursion
            (goto-char (point-at-bol))
            (insert line))
          (esn-number-est what))))))

;;;###autoload
(defun esn-renumber-theta ()
  "Renumber the THETAs"
  (interactive)
  (when esn-wfn-extended
    (esn-fix-numbering))
  (esn-renumber "THETA")
  (when esn-wfn-extended
    (esn-undo-numbering)))

;;;###autoload
(defun esn-renumber-eta ()
  (interactive)
  (when esn-wfn-extended
    (esn-fix-numbering))
  (esn-renumber "ETA")
  (when esn-wfn-extended
    (esn-undo-numbering)))

;;;###autoload
(defun esn-renumber-eps ()
  (interactive)
  (when esn-wfn-extended
    (esn-fix-numbering))
  (esn-renumber "EPS")
  (when esn-wfn-extended
    (esn-undo-numbering)))

;;;###autoload
(defun esn-renumber-err ()
  (interactive)
  (when esn-wfn-extended
    (esn-fix-numbering))
  (esn-renumber "ERR")
  (when esn-wfn-extended
    (esn-undo-numbering)))

;;;###autoload
(defun esn-renumber-mtime ()
  (interactive)
  (esn-renumber "MTIME"))

;;;###autoload
(defun esn-renumber-mpast ()
  (interactive)
  (esn-renumber "MPAST"))

;;;###autoload
(defun esn-renumber-mnext ()
  (interactive)
  (esn-renumber "MNEXT"))

;;;###autoload
(defun esn-renumber-dadt ()
  (interactive)
  (esn-renumber "DADT"))

;;;###autoload
(defun esn-renumber-a ()
  (interactive)
  (esn-renumber "A"))

;;;###autoload
(defun esn-renumber-p ()
  (interactive)
  (esn-renumber "P"))

;;;###autoload
(defun esn-renumber-cden_ ()
  (interactive)
  (esn-renumber "CDEN_"))

;;;###autoload
(defun esn-renumber-a_0 ()
  (interactive)
  (esn-renumber "A_0"))

;;;###autoload
(defun esn-renumber-thsimp ()
  (interactive)
  (esn-renumber "THSIMP"))

;;;###autoload
(defun esn-renumber-omsimp ()
  (interactive)
  (esn-renumber "OMSIMP"))

;;;###autoload
(defun esn-renumber (what &optional below)
  "Renumbers X and fills in the gaps, say X=com.  Returns the max com if below isn't specified.  Otherwise returns a list of  max com and number of missing items below the variable below."
  (interactive (list (read-string "What variable to renumber: ")))
  (let (
        (debug-on-error t)
        (debug-on-quit t)
        (inhibit-read-only 't)
        (inhibit-point-motion-hooks 't)
        (case-fold-search 't)
        (mx (esn-max-what what))
        (i 0)
        (last 1)
        (count 0)
        (lst '())
        (md nil))
    (while (< i mx)
      (message "i:%s, l: %s, mx: %s" i last mx)
      (setq i (+ i 1))
      (save-excursion
        (goto-char (point-min))
        (if (and
             (re-search-forward (format "\\<%s(%s)" what i) nil t)
             (not (string= "TAB" (esn-get-current-record))) ; Table information removed.
             (not (esn-in-comment-p)) ; Not in a comment.
             )
            (progn
              (goto-char (point-min))
              (while (re-search-forward (format "\\<%s(%s)" what i) nil t)
                (unless (string= "TAB" (esn-get-current-record))
                  (replace-match (format "%s(%s)" what last) t t)))
              (setq last (+ last 1)))
          (if below
              (if (<= i below)
                  (setq count (+ 1 count)))))))
    (setq last (- last 1))
    (setq i last)
    (while (< i mx)
      (setq i (+ i 1))
      (message "Removing %s(%s)" what i)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (format "\\(?:  +\\)?\\(?:[A-Z0-9]+ *= *\\)?\\<%s(%s)\\(?: *= *[A-Z0-9]+\\)?\\(?:  +\\)?" what i) nil t)
          (replace-match ""))))
    (if below
        (progn
          (setq lst (list last count))
          (symbol-value 'lst))
      (symbol-value 'last))))

;;;###autoload
(defun esn-file-exists-p (file)
  "Determines if file exists using absolute directory for file based on the NONMEM run options."
  (let (
        (ret (save-match-data (file-exists-p (esn-get-abs-dir file)))))
    (symbol-value 'ret)))


;;;###autoload
(defun esn-get-abs-dir (file)
  "Gets the absolute directory for file based on the NONMEM run options."
  (let (
        (case-fold-search 't)
        (tmp file)
        (fni (buffer-file-name))
        (saved (assoc file esn-abs-dir-hash)))
    (if saved
        (nth 1 saved)
      (if (not fni)
          (symbol-value 'file)
        (when tmp
          (while (and (and esn-w32 esn-fix-windows-slashes) (string-match "/" fni))
            (setq fni (replace-match "\\\\" nil nil fni)))
          (while (string-match "^[ \t\n]+" fni)
            (setq fni (replace-match "" nil nil fni)))
          (if (string-match "\\([\\/\\\\]\\)[^\\\\\\/]*$" fni)
              (setq fni (replace-match "\\1" nil nil fni)))
          (if (string-match "^['\"]+" tmp)
              (setq tmp (replace-match "" nil nil tmp)))
          (if (string-match "['\"]*)?$" tmp)
              (setq tmp (replace-match "" nil nil tmp)))
          (if (string-match "^\\(\\/\\|[A-Za-z]:[\\\\\\/]\\)" tmp) nil
            (if (string-match (format "^%s" (regexp-quote esn-completing-current-directory)) tmp)
                (setq tmp (replace-match "" nil nil tmp)))
            (while (string-match "^\\.\\.[\\\\/]" tmp)
              (setq tmp (replace-match "" nil nil tmp))
              (if (string-match "\\([\\\\/]\\)[^\\\\/]*[\\\\/]$" fni)
                  (setq fni (replace-match "\\1" nil nil fni))))
            (setq tmp (concat fni tmp))))
        (add-to-list 'esn-abs-dir-hash (list file tmp))
        (symbol-value 'tmp)))))

;;;###autoload
(defun esn-get-nonmem-output (&optional exts)
  "Get the NONMEM run output if it exists."
  (let (
        (fn (buffer-file-name))
        (fn2 (buffer-file-name))
        (outf nil)
        (tmp nil))
    (when fn
      (when (string-match (regexp-opt esn-default-extension 't) fn)
        (setq fn (replace-match "" nil nil fn))
        (mapc (lambda(ext)
                (when (and fn ext (file-exists-p (concat fn ext)))
                  (setq outf (concat fn ext)))
                (when (and fn2 ext (file-exists-p (concat fn2 ext)))
                  (setq outf (concat fn2 ext))))
              (or exts esn-nonmem-output-extension)))
      ;; Fix case of the output files (in windows it could not be the right case.)
      (when outf
        (setq tmp outf)
        (when (string-match "[/\\\\][^/\\\\]*$" tmp)
          (setq tmp (replace-match "" nil nil tmp)))
        (setq tmp (directory-files tmp 't))
        (mapc (lambda(x)
                (when (string= (downcase x) (downcase outf))
                  (setq outf x)))
              tmp
              )))
    (symbol-value 'outf)))

;;;###autoload
(defun esn-get-output-files (&optional addOutput)
  "Gets the output files for the current NONMEM run.  If addOutput is true, include the NONMEM run output, R script, and R outout file if they exists"
  (interactive)
  (let (
        (case-fold-search 't)
        (out-file '())
        (other-files '())
        (tmp "")
        (fn (buffer-file-name))
        end beg over
        )
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "[ \n]\\(?:FILE? *[= \t] *\\|MSFO? *[= \t] *\\|OPEN *( *[0-9]+ *, *FILE *= *\\)\\([^ \n\t]*\\)" nil t)
        (add-to-list 'out-file (esn-get-abs-dir (match-string 1))))
      (goto-char (point-min))

      (while (re-search-forward "\\<\\(?:OTH\\(?:ER?\\)?\\|PK\\|ERR\\(?:OR?\\)?\\|MODEL?\\|DES\\|AES\\|TOL\\|INFN?\\|PRED\\|CRIT\\|MIX\\|PRIOR?\\|CONTR?\\|CCON\\(?:TR?\\)?\\|USME\\(?:TA?\\)?\\|SPTWO?\\|INLE\\(?:TA?\\)?\\) *[ \t=] *\\([^ \n\t]+\\)" nil t)
        (setq tmp (esn-get-abs-dir (match-string 1)))
        (when (and tmp (file-exists-p tmp))
          (add-to-list 'other-files tmp)))
                                        ; Now get other files outputs.
      (mapc (lambda (file)
              (find-file file)
              (goto-char (point-min))
              (while (re-search-forward "[ \n]\\(?:FILE? *[= \t] *\\|MSFO? *[= \t] *\\|OPEN *( *[0-9]+ *, *FILE *= *\\)\\([^ \n\t]+\\)" nil t)
                (add-to-list 'out-file (esn-get-abs-dir (match-string 1))))
              (kill-buffer (current-buffer)))
            other-files)
      (when addOutput
        (add-to-list 'out-file (esn-get-nonmem-output))
        (add-to-list 'out-file (esn-get-nonmem-output (list ".R" ".r" ".s" ".sas" ".SAS" ".q" ".Q")))
        (add-to-list 'out-file (esn-get-nonmem-output (list ".Rout")))
        (add-to-list 'out-file (esn-get-nonmem-output (list ".txt" ".cov" ".coi" ".cor" ".phi")))
                                        ;       (add-to-list 'out-file (esn-get-nonmem-output (list ".rtf"))))
        (symbol-value 'out-file)))))

;;;###autoload
(defun esn-get-input-files ()
  "Gets the input files for the current NONMEM run."
  (interactive)
  (let (
        (case-fold-search 't)
        (out-file '())
        (tmp "")
        (tmp2 ""))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\(?:\\(?:\\$\\(?:MSFI?\\|DATA?\\)\\|\\$?INC\\(?:L\\(?:U\\(?:DE?\\)?\\)?\\)?\\) +\\|\\(?:\\<\\(?:OTH\\(?:ER?\\)?\\|PK\\|ERR\\(?:OR?\\)?\\|MODEL?\\|DES\\|AES\\|TOL\\|INFN?\\|PRED\\|CRIT\\|MIX\\|PRIOR?\\|CONTR?\\|CCON\\(?:TR?\\)?\\|USME\\(?:TA?\\)?\\|SPTWO?\\|INLE\\(?:TA?\\)?\\) *= *\\)\\)\\([^ \n\t]+\\)" nil t)
        (when (match-string 1)
          (setq tmp2 (match-string 0))
          (setq tmp (esn-get-abs-dir (match-string 1)))
          (unless (string-match "TOL *= *[0-9]+" tmp2)
            (add-to-list 'out-file tmp))))
      (symbol-value 'out-file))))

;;;###autoload
(defun esn-command-to-string (cmd)
  "Executes a command in Emacs or Xemacs (doesn't matter). If at first the command fails, try 3 more times.

I have encountered the following error before:

shell-command-to-string: Creating process pipe: no error

Not sure what to do about it but try again.

"
  (let (
        (command cmd)
        (ret nil)
        (i 0))
    (while (< i 3)
      (condition-case nil
          (progn
            (when esn-w32
              (when (file-exists-p (concat esn-path "bin/" (nth 0 (split-string command)) ".exe" ))
                (setq command (concat esn-path "bin/" command))))
            (if (fboundp 'shell-command-to-string)
                (setq ret (shell-command-to-string command))
              (setq ret (exec-to-string command)))
            (setq i 100))
        (error nil))
      (setq i (+ i 1))
      (when (< i 3)
        (sleep-for 2) ;; Sleep for 2 seconds before trying again.
        ))
    (symbol-value 'ret)))

;;;###autoload
(defun esn-prompt (prompt &optional
                          reg altPrompt default
                          txt
                          )
  "Prompts for something that optionally is forced to match reg"
  (let (
        (txt (read-string prompt txt nil default)))
    (when reg
      (while (not (string-match reg txt))
        (if (and default (string= "" txt))
            (setq txt default)
          (ding)
          (setq txt (read-string (or altPrompt prompt)
                                 txt nil default
                                 )))))
    (symbol-value 'txt)))

;;;###autoload
(defun esn-mangle-problem-file-name (name problem)
  "This changes file names to unique names based on the problem that you are in.  For example sdtab1 is changed to sdtab1_2 in the second problem statement."
  (let (
        (ret name))
    (if (<= problem 1)
        (symbol-value 'name)
      (if (string-match "[.]\\([^.]*\\)" ret)
          (setq ret (replace-match (format "_%s.\\1" problem) nil nil ret))
        (setq ret (format "%s_%s" ret problem)))
      (symbol-value 'ret))))

;;;###autoload
(defun esn-format (text &rest args)
  "* Provides esn-format which formats everything the same way as format with the exception of %1% %2%, etc.  Those are replaced with the first format argument, and then the second, and so on."
  (if (and text args)
      (let (val valn)
        (while (string-match "%\\([1-9][0-9]*\\)%" text)
          (setq val (match-string 1 text))
          (setq valn (- (string-to-number val) 1))
          (while (string-match (regexp-quote (concat "%" (match-string 1 text) "%")) text)
            (setq text (replace-match (format "%s" (nth valn args)) 't 't text))))
        (setq text (apply 'format text args))))
  (symbol-value 'text))

(provide 'esn-fun)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-fun.el ends here
