;;; esn-completion.el ---  Completion routines that work with light.el
;;
;; Filename: esn-completion.el
;; Description: Lighting completion routines.
;; Author: Matthew L. Fidler
;; Maintainer: Matthew L. Fidler
;; Created: Thu Jan 28 13:17:39 2010 (-0600)
;; Version: 0.1
;; Last-Updated:
;;           By:
;;     Update #: 185
;; URL: http://esnm.sourceforge.net
;; Keywords: Emacs Speaks NONMEM
;; Compatibility: Emacs 23.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Completion subroutines.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;; 17-Jan-2011    Matthew L. Fidler  
;;    Added IOV tab completion without having to type IOV# = [TAB].  Just type IOV#[TAB].  For example:
;;
;;  CL=TVCL*DEXP(ETA(1)+IOV#[TAB])
;;
;; 06-Oct-2010    Matthew L. Fidler  
;;    Added IOV tab completion.
;; 20-Sep-2010    Matthew L. Fidler  
;;    Updated esn-started-completion-engine variable on finished completion.
;; 07-Sep-2010    Matthew L. Fidler  
;;    Bug fix when using generalized Emacs completion
;; 30-Aug-2010    Matthew L. Fidler  
;;    Moved variables to esn-vars
;; 23-Aug-2010    Matthew L. Fidler  
;;    Drastically changed completion routines
;; 19-Aug-2010    Matthew L. Fidler  
;;    Tried to standardize record regular expressions.
;; 28-Jul-2010    Matthew L. Fidler  
;;    Added completion of GRD=DDDDDSDDD based on if THETA(x) is in the $ERROR
;;    block.
;; 21-Jul-2010    Matthew L. Fidler  
;;    Added easier FIXED completion on THETA records 
;; 12-Jul-2010    Matthew L. Fidler
;;    Added SEED=##### when completion of $SIM is enabled.
;; 28-Jan-2010    Matthew L. Fidler
;;    Revised to allow smart capitalization during completion.
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

(declare-function esn-get-current-record "esn-narrow")
(declare-function esn-get-current-rec "esn-narrow")
(declare-function esn-scale "esn-units")
(declare-function esn-iov "esn-iov")
(declare-function esn-iov2 "esn-iov")
(declare-function esn-add-input-line "esn-input")
(declare-function esn-narrow-rec "esn-narrow")
(declare-function esn-is-abbrev-p "esn-narrow")
(declare-function esn-message "esn-exec")
(declare-function esn-max-theta "esn-properties")


(eval-when-compile
  (require 'esn-start))

(defgroup esn-completion nil
  "* EsN completion options"
  :group 'esn-changes-while-editing)

(defcustom esn-completion-type (if (featurep 'auto-complete)
                                   2
                                 (if (featurep 'company)
                                     1
                                   (if (featurep 'completion-ui)
                                       3
                                     999)))
  "* Use company completion when available."
  :type '(choice 
          (integer :tag "No completion" 0)
          (integer :tag "Company-Mode completion" 1)
          (integer :tag "Auto-complete mode completion" 2)
          (integer :tag "Completion UI autocompletion" 3)
          (integer :tag "Emacs default completion" 999))
  :group 'esn-completion)

(defcustom esn-grd-complete 't
  "* Defines if GRD= will be completed with DDDDDDS where S signifies the sigma-like THETAs."
  :type 'boolean
  :group 'esn-completion
  :group 'esn-$estimate-options)

(defcustom esn-scale-complete 't
  "* Defines if SC =  will be completed with appropriate scale factor (after prompting for units). See `esn-units' for more options about scale completion."
  :type 'boolean
  :group 'esn-completion
  :group 'esn-$pk-options
  :group 'esn-$pred-options)


(defun esn-complete-is-known-option-value (rec opt what)
  "* Determines if the completion is a known record option value"
  ;; Values, Requires a space or equals
  (let ((opt-val (esn-rec3 (concat (upcase opt) "=")))
        tmp
        ret)
    (setq tmp (assoc rec esn-current-record-complete-option-values))
    (when tmp
      (setq tmp (cadr tmp))
      (setq tmp (assoc opt-val tmp))
      (when tmp
        (setq ret (all-completions what (cadr tmp)))))
    (symbol-value 'ret)))

(defun esn-complete-is-known-record (what)
  "* Determines if the completion is a known record"
  (all-completions what esn-current-records-complete))

(defun esn-complete-is-known-option (rec what &optional no-append)
  "* Determines if the completion is a known record option"
  (let (opt ret)
    (when what
      (setq opt (assoc rec esn-current-record-complete-options))
      (when opt
        (setq opt (cadr opt))
        (setq ret (all-completions what (append opt (if no-append '() esn-current-records-complete))))))
    (symbol-value 'ret)))

(defun esn-after-completion (&optional what)
  "* Function run after completion"
  (when what
    (let (
          (case-fold-search 't)
          (endcol 0)
          (rec (esn-get-current-record)))
      (cond
       ( (and esn-sim-add-seed (string-match "\\<SEED\\>[ \t=]" (upcase what)))
         (insert (esn-gen-seed)))
       ( (and esn-grd-complete (string-match "\\<GRD\\>[ \t=]" (upcase what)))
         (insert (save-excursion (esn-complete-grd))))
       ( (and 
          (string-match "\\<FIX\\(ED?\\)?\\>" (upcase what))
          (or 
           (string= "THE" (esn-get-current-rec))
           (string= "OME" (esn-get-current-rec))
           (string= "SIG" (esn-get-current-rec))))
         (esn-make-fixed-theta))
       ( (and esn-scale-complete 
              (or 
               (string= "PK" (esn-get-current-rec))
               (string= "PRE" (esn-get-current-rec)))
              (looking-back "^[ \t]*\\<S\\([0-9]+\\|C\\)[ \t=]*"))
         (esn-scale))
       ( (and esn-tab-complete-iov
              (string-match "\\<IOV[0-9]+[ \t]*=" (upcase what)))
         (esn-iov))
       ( (and esn-tab-complete-iov
              (string-match "\\<IOV[0-9]+\\>" (upcase what)))
         (esn-iov2))
       ( (and (string-match (regexp-quote "$INPUT") (upcase what)) (looking-at "[ \t]*$"))
         (when (looking-back (regexp-quote "$INPUT"))
           (insert " "))
         (esn-add-input-line)
         (when (string= "INP" (esn-get-current-record))
           (save-restriction
             (esn-narrow-rec)
             (goto-char (point-max)))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables previously defined from point.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun esn-complete-defined-variables ()
  "* Returns a list of all the previously defined variables for this line"
  (interactive)
  (save-excursion
    (let ((ret '())
          tmp)
      (beginning-of-line)
      (while (re-search-backward "^[ \t]*\\([A-Z0-9a-z_]+\\)[ \t]*=" nil t)
        (setq tmp (upcase (match-string 1)))
        (when (esn-is-abbrev-p)
          (add-to-list 'ret tmp) ;; Add abbreviated records definitions.
          ))
      (when (interactive-p)
        (esn-message "%s" ret))
      (symbol-value 'ret))))

(defun esn-get-comment-wrapped-values (val extra chrlen)
  "Wraps based on chars position.  This is to wrap comments that
are completed so they don't go over the `esn-character-limit'
character limit."
  (let* ((tackon " ;")
         (spl (split-string extra "[, \t\n;]+"))
         (i 0)
         (len (length spl))
         (word "")
         (tmp (concat val tackon))
         (chars (+ chrlen (length tmp)))
         (curlen (- chars 1))
         (case-fold-search 't))
    (setq tmp (concat tmp " "))
    (setq chars (- chars 1))
    (setq curlen (+ chars 1))
    (setq tackon "")
    (while (< i len)
      (setq word (concat (nth i spl) " "))
      (if (string-match "^[, \t]+$" word)
          (progn )
        (if (> esn-character-limit (+ curlen (length word)))
            (progn
              (setq tackon (concat tackon word))
              (setq curlen (+ curlen (length word))))
          (setq curlen (+ (+ chars 1) (length word)))
          ;; take off space
          (setq tackon (substring tackon 0 -1))
          (setq tackon (concat tackon "\n" (make-string (- chars 1) ? ) "; " word))))
      (setq i (+ i 1)))
    (setq tmp (concat tmp tackon))
    (setq tmp (substring tmp 0 -1))
    (setq tmp (concat tmp "\n"))
    (symbol-value 'tmp)))

(defun esn-make-fixed-theta ()
  "* Takes out initial estimates and ending estimates for FIXED theta, that is:
The estimate (0, 1, FIXED) becomes (1, FIXED) and (0, 1, 3, FIXED) becomes (1, FIXED)
"
  (interactive)
  (save-match-data
    (save-excursion
      (save-restriction
        (let ((case-fold-search 't)
              pt1 pt2)
          (cond 
           ( (re-search-backward "([^)]*\\=" nil t)
             (cond 
              ( (looking-at "([ ,\t]*[0-9.Ee+-]+[ ,\t]+\\([0-9.Ee+-]+\\)\\(?:[ ,\t]+[0-9.Ee+-]+\\)?[ ,\t]+FIX\\(?:ED?\\)?[ ,\t]*)")
                (replace-match "(\\1, FIXED)"))))
           ( (re-search-backward "([^)]*)[ \t]*FIXED\\=" nil t)
             (cond
              ( (looking-at "([ ,\t]*\\([0-9.Ee+-]+\\)[ ,\t]*)[ \t]*FIX\\(?:ED?\\)?")
                (replace-match "(\\1, FIXED)"))
              ( (looking-at "([ ,\t]*[0-9.Ee+-]+[ ,\t]+\\([0-9.Ee+-]+\\)\\(?:[ ,\t]+[0-9.Ee+-]+\\)?[ ,\t]*)[ \t]*FIX\\(?:ED?\\)?")
                (replace-match "(\\1, FIXED)"))))))))))

(defun esn-complete-grd ()
  "* Completes GRD=DDDDS as needed."
  (save-excursion
    (let (
          (ntheta (esn-max-theta))
          (case-fold-search 't)
          tmp
          (s-thetas '())
          (i 1)
          (grd '())
          (ret ""))
      (save-restriction
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "\\<[$]ERR[A-Z]*\\>" nil t)
            (esn-narrow-rec)
            (setq tmp (buffer-substring (point-min) (point-max))))))
      (when tmp
        (with-temp-buffer
          (insert tmp)
          (goto-char (point-min))
          (while (re-search-forward ";.*" nil t)
            (replace-match ""))
          (goto-char (point-min))
          (while (re-search-forward "\\<THETA(\\([0-9]+\\))" nil t)
            (add-to-list 's-thetas (string-to-number (match-string 1))))
          (while (<= i ntheta)
            (if (memq i s-thetas)
                (push "S" grd)
              (push "D" grd))
            (setq i (+ i 1)))
          (setq ret (mapconcat (lambda (x) x) (reverse grd) ""))))
      (symbol-value 'ret))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion prefix used for comit, copany and completion ui
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun esn-ac-source-available-p (source)
  ;; Stole from auto-completion source.
  (if (and (featurep 'auto-complete)
	   (symbolp source)
           (get source 'available)
	   (fboundp #'ac-source-entity))
      (eq (get source 'available) t)
    (let* ((src (ac-source-entity source))
           (avail-pair (assq 'available src))
           (avail-cond (cdr avail-pair))
           (available (and (if avail-pair
                               (cond
                                ((symbolp avail-cond)
                                 (funcall avail-cond))
                                ((listp avail-cond)
                                 (eval avail-cond)))
                             t)
                           (loop for feature in (assoc-default 'depends src)
                                 unless (require feature nil t) return nil
                                 finally return t))))
      (if (symbolp source)
          (put source 'available (if available t 'no)))
      available)))

(defun esn-complete-prefix (&optional exit prefix)
  "Defines the prefix to be used with completion."
  (interactive)
  (let (ret (debug-on-error t))
    (block searching-prefixes
      (mapc (lambda(source-name)
	      (let* ((source (symbol-value source-name))
                     (prefix (assoc-default 'prefix source))
                     (requires (assoc-default 'requires source))
                     (init (assoc-default 'init source))
                     (cand (assoc-default 'candidates source))
		     tmp-ret)
                (when (listp requires)
                  (setq requires (car requires)))
                (when cand
                  (when (looking-back prefix)
                    (setq tmp-ret (or (match-string 1) (match-string 0)))
                    (when (<= (or requires 1) (length tmp-ret))
                      (when (esn-ac-source-available-p source)
                        ;; Has sufficient length.
                        (when init
                          (funcall init))
                        (setq cand (all-completions tmp-ret
                                                    (cond ((functionp cand)
                                                           (funcall cand))
                                                          (t
                                                           (eval cand)))))
                        (when (<= 1 (length cand))
                          (setq ret tmp-ret)
                          (setq esn-current-completions cand)
                          (return-from searching-prefixes))))))))
            esn-ac-sources))
    (symbol-value 'ret)))


(defun esn-completion-off ()
  "Turn off completion in EsN buffer."
  (interactive)
  (when (and (fboundp 'company-mode) (bound-and-true-p company-mode))
    (company-mode -1))
  (when (and (fboundp 'auto-complete-mode) (bound-and-true-p auto-complete-mode))
    (auto-complete-mode -1))
  (when (and (fboundp 'auto-completion-mode) (bound-and-true-p auto-completion-mode))
    (auto-completion-mode -1)))


(provide 'esn-completion)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esn-completion.el ends here
